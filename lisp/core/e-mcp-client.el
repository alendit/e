;;; e-mcp-client.el --- MCP catalog and client semantics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns remembered server identity, catalog caching, transport-independent
;; list/call/refresh semantics, and bounded catalog discovery.  Physical
;; helper and HTTP state belongs to the transport owners.

;;; Code:

(require 'cl-lib)
(require 'e-tools)
(require 'e-mcp-protocol)
(require 'e-mcp-stdio)
(require 'e-mcp-http)

(defconst e-mcp-client--missing-catalog (make-symbol "e-mcp-missing-catalog")
  "Sentinel used to distinguish absent cache entries from empty catalogs.")

(defvar e-mcp-client--known-servers nil
  "MCP servers observed during capability construction.")

(defvar e-mcp-client--catalog-cache (make-hash-table :test 'equal)
  "Memoized tools/list catalogs keyed by sorted server id list.
Progressive disclosure touches each server catalog from several places per
turn (Tier-0 card, Tier-1 resources, Tier-2 activation, lazy tool
registration).  Memoizing the helper round trip keeps that fan-out from
re-listing tools on every turn.  Invalidated by `e-mcp-reset' and
`e-mcp-refresh'.")

(defvar e-mcp-client--catalog-starts (make-hash-table :test 'equal)
  "In-flight async MCP catalog discovery requests.
Keys match `e-mcp-client--catalog-cache'.")


(defun e-mcp-client--catalog-key (servers)
  "Return a stable cache key for SERVERS."
  (sort (mapcar #'e-mcp-server-id servers) #'string<))

(defun e-mcp-client--catalog-cache-entry (servers)
  "Return cached catalog for SERVERS, or the missing sentinel."
  (gethash (e-mcp-client--catalog-key servers)
           e-mcp-client--catalog-cache
           e-mcp-client--missing-catalog))

(defun e-mcp-client--catalog-cached-p (servers)
  "Return non-nil when SERVERS has a cached catalog entry."
  (not (eq (e-mcp-client--catalog-cache-entry servers)
           e-mcp-client--missing-catalog)))

(defun e-mcp-client--invalidate-catalog (servers)
  "Drop any memoized catalog for SERVERS."
  (let ((key (e-mcp-client--catalog-key servers)))
    (remhash key e-mcp-client--catalog-cache)
    (remhash key e-mcp-client--catalog-starts)))

(defun e-mcp-client--list-tools-uncached (servers)
  "Return freshly discovered MCP tools for SERVERS, bypassing the cache.
HTTP servers are handled in-process; stdio servers use the helper."
  (let ((http-servers (cl-remove-if-not #'e-mcp-protocol-http-server-p servers))
        (stdio-servers (cl-remove-if #'e-mcp-protocol-http-server-p servers))
        tools)
    ;; HTTP transport: direct Elisp
    (dolist (server http-servers)
      (setq tools (append tools (e-mcp-http-list-tools server))))
    ;; stdio transport: Node helper
    (when stdio-servers
      (dolist (tool (e-mcp-stdio-list-tools stdio-servers))
        (push tool tools)))
    (nreverse tools)))

(defun e-mcp-list-tools (servers)
  "Return discovered MCP tools for SERVERS.
The catalog is memoized per server set; `e-mcp-refresh' or `e-mcp-reset'
invalidate it."
  (let ((cached (e-mcp-client--catalog-cache-entry servers)))
    (if (not (eq cached e-mcp-client--missing-catalog))
        cached
      (puthash (e-mcp-client--catalog-key servers)
               (e-mcp-client--list-tools-uncached servers)
               e-mcp-client--catalog-cache))))

(cl-defun e-mcp-list-tools-start
    (servers &key on-done on-error on-event &allow-other-keys)
  "Start discovering MCP tools for SERVERS asynchronously.
ON-DONE receives the discovered tools and the catalog cache is populated before
the callback runs."
  (let* ((key (e-mcp-client--catalog-key servers))
         (cached (e-mcp-client--catalog-cache-entry servers)))
    (if (not (eq cached e-mcp-client--missing-catalog))
        (let ((settled nil)
              timer)
          (setq timer
                (run-at-time
                 0 nil
                 (lambda ()
                   (unless settled
                     (setq settled t)
                     (when on-done
                       (funcall on-done cached))))))
          (e-tools-request-create
           :cancel (lambda ()
                     (unless settled
                       (setq settled t)
                       (when (timerp timer)
                         (cancel-timer timer)))
                     t)
           :metadata '(:transport timer
                       :kind mcp-list-tools
                       :source cache
                       :cancellable queued-only)))
      (let* ((http-servers (cl-remove-if-not #'e-mcp-protocol-http-server-p servers))
             (stdio-servers (cl-remove-if #'e-mcp-protocol-http-server-p servers))
             (slot-count (+ (length http-servers)
                            (if stdio-servers 1 0)))
             (slots (make-vector slot-count nil))
             child-requests
             (pending slot-count)
             settled)
        (cl-labels
            ((cancel-children ()
               (dolist (request child-requests)
                 (e-tools-cancel-request request)))
             (fail (condition)
               (unless settled
                 (setq settled t)
                 (cancel-children)
                 (when on-error
                   (funcall on-error condition))))
             (finish-slot (index tools)
               (unless settled
                 (aset slots index tools)
                 (setq pending (1- pending))
                 (when (= pending 0)
                   (let ((catalog
                          (apply #'append (append slots nil))))
                     (puthash key catalog e-mcp-client--catalog-cache)
                     (setq settled t)
                     (when on-done
                       (funcall on-done catalog))))))
             (remember (request)
               (push request child-requests)
               request))
          (when (= pending 0)
            (let ((catalog nil))
              (puthash key catalog e-mcp-client--catalog-cache)
              (setq settled t)
              (when on-done
                (funcall on-done catalog))))
          (cl-loop for server in http-servers
                   for index from 0
                   do
                   (let ((slot index))
                     (condition-case condition
                         (remember
                          (e-mcp-http-list-tools-start
                           server
                           :on-done (lambda (tools)
                                      (finish-slot slot tools))
                           :on-error #'fail
                           :on-event on-event))
                       (error
                        (fail condition)))))
          (when stdio-servers
            (let ((index (length http-servers)))
              (condition-case condition
                  (remember
                   (e-mcp-stdio-list-tools-start
                    stdio-servers
                    :on-done (lambda (result)
                               (finish-slot index result))
                    :on-error #'fail
                    :on-event on-event))
                (error
                 (fail condition)))))
          (e-tools-request-create
           :cancel (lambda ()
                     (unless settled
                       (setq settled t)
                       (cancel-children))
                     t)
           :metadata (list :transport 'aggregate
                           :kind 'mcp-list-tools
                           :server-count (length servers)
                           :cancellable 'cancel-children)))))))

(defun e-mcp-client--warn-server-failure (server err)
  "Emit a warning that SERVER discovery failed with ERR, then continue."
  (display-warning
   'e-mcp
   (format "MCP server %s unavailable, skipping: %s"
           (e-mcp-server-id server)
           (e-work-error-message err))
   :warning))

(defun e-mcp-client--ensure-catalog-started (servers)
  "Start async catalog discovery for SERVERS unless cached or already in flight."
  (let ((key (e-mcp-client--catalog-key servers)))
    (unless (or (e-mcp-client--catalog-cached-p servers)
                (gethash key e-mcp-client--catalog-starts))
      (condition-case err
          (let* ((done (lambda (_catalog)
                         (remhash key e-mcp-client--catalog-starts)))
                 (failed (lambda (condition)
                           (remhash key e-mcp-client--catalog-starts)
                           (when (= (length servers) 1)
                             (e-mcp-client--warn-server-failure
                              (car servers) condition))))
                 (request (e-mcp-list-tools-start
                           servers
                           :on-done done
                           :on-error failed)))
            (puthash key request e-mcp-client--catalog-starts)
            request)
        (e-mcp-backend-error
         (remhash key e-mcp-client--catalog-starts)
         (when (= (length servers) 1)
           (e-mcp-client--warn-server-failure (car servers) err))
         nil)))))

(defun e-mcp-client-catalogs-cached (servers &optional start-missing)
  "Return cached (SERVER . CATALOG) pairs for SERVERS.
When START-MISSING is non-nil, begin async discovery for missing catalogs."
  (let (pairs)
    (dolist (server servers)
      (let* ((single (list server))
             (cached (e-mcp-client--catalog-cache-entry single)))
        (if (not (eq cached e-mcp-client--missing-catalog))
            (push (cons server cached) pairs)
          (when start-missing
            (e-mcp-client--ensure-catalog-started single)))))
    (nreverse pairs)))

(defun e-mcp-client-tools-cached (servers &optional start-missing)
  "Return cached discovered tools for SERVERS.
When START-MISSING is non-nil, begin async discovery for missing catalogs."
  (apply #'append
         (mapcar #'cdr
                 (e-mcp-client-catalogs-cached servers start-missing))))

(defun e-mcp-client-catalogs (servers)
  "Return a list of (SERVER . CATALOG) for SERVERS that discover successfully.
Servers whose discovery signals an `e-mcp-backend-error' are logged and
omitted so a single broken MCP server cannot block harness startup."
  (let (pairs)
    (dolist (server servers)
      (condition-case err
          (push (cons server (e-mcp-list-tools (list server))) pairs)
        (e-mcp-backend-error
         (e-mcp-client--warn-server-failure server err))))
    (nreverse pairs)))

(defun e-mcp-client-tools (servers)
  "Return discovered tools for SERVERS, skipping servers that fail discovery."
  (apply #'append (mapcar #'cdr (e-mcp-client-catalogs servers))))

(defun e-mcp-call-tool (servers server-id tool-name arguments)
  "Call TOOL-NAME on SERVER-ID through SERVERS with ARGUMENTS."
  (let ((server (cl-find server-id servers
                         :key #'e-mcp-server-id :test #'equal)))
    (unless server
      (signal 'e-mcp-backend-error
              (list (format "Unknown MCP server: %s" server-id))))
    (if (e-mcp-protocol-http-server-p server)
      (e-mcp-http-call-tool server tool-name arguments)
      (e-mcp-stdio-call-tool
       (cl-remove-if #'e-mcp-protocol-http-server-p servers)
       server-id tool-name arguments))))

(cl-defun e-mcp-call-tool-start
    (servers server-id tool-name arguments
             &key on-done on-error on-event &allow-other-keys)
  "Start TOOL-NAME on SERVER-ID through SERVERS with ARGUMENTS."
  (let ((server (cl-find server-id servers
                         :key #'e-mcp-server-id :test #'equal)))
    (unless server
      (signal 'e-mcp-backend-error
              (list (format "Unknown MCP server: %s" server-id))))
    (if (e-mcp-protocol-http-server-p server)
        (e-mcp-http-call-tool-start
         server tool-name arguments
         :on-done on-done
         :on-error on-error
         :on-event on-event)
      (e-mcp-stdio-call-tool-start
       (cl-remove-if #'e-mcp-protocol-http-server-p servers)
       server-id tool-name arguments
       :on-done on-done
       :on-error on-error
       :on-event on-event))))

(defun e-mcp-refresh (&optional servers)
  "Refresh tool catalogs for SERVERS.
HTTP servers are refreshed in-process; stdio servers use the helper.
Interactively, refresh all servers seen during capability construction."
  (interactive)
  (let ((servers (or servers e-mcp-client--known-servers)))
    (unless servers
      (signal 'e-mcp-backend-error
              (list "No MCP servers are configured for refresh")))
    (if (called-interactively-p 'interactive)
        (progn
          (e-mcp-refresh-start
           servers
           :on-done (lambda (_result)
                      (message "MCP refresh finished"))
           :on-error (lambda (err)
                       (display-warning 'e-mcp
                                        (e-work-error-message err)
                                        :warning)))
          nil)
      (e-mcp-client--invalidate-catalog servers)
      (let ((http-servers (cl-remove-if-not #'e-mcp-protocol-http-server-p servers))
            (stdio-servers (cl-remove-if #'e-mcp-protocol-http-server-p servers)))
        (dolist (server http-servers)
          (e-mcp-http-refresh server))
        (when stdio-servers
          (e-mcp-stdio-refresh stdio-servers))))))

(cl-defun e-mcp-refresh-start
    (&optional servers &key on-done on-error on-event &allow-other-keys)
  "Start an MCP catalog refresh for SERVERS asynchronously."
  (let* ((servers (or servers e-mcp-client--known-servers))
         (http-servers (cl-remove-if-not #'e-mcp-protocol-http-server-p servers))
         (stdio-servers (cl-remove-if #'e-mcp-protocol-http-server-p servers))
         child-requests
         (pending 0)
         settled)
    (unless servers
      (signal 'e-mcp-backend-error
              (list "No MCP servers are configured for refresh")))
    (e-mcp-client--invalidate-catalog servers)
    (cl-labels
        ((cancel-children ()
           (dolist (request child-requests)
             (e-tools-cancel-request request)))
         (fail (condition)
           (unless settled
             (setq settled t)
             (cancel-children)
             (when on-error
               (funcall on-error condition))))
         (finish-one (_result)
           (unless settled
             (setq pending (1- pending))
             (when (= pending 0)
               (setq settled t)
               (when on-done
                 (funcall on-done '(:refreshed t))))))
         (remember (request)
           (push request child-requests)
           request))
      (dolist (server http-servers)
        (setq pending (1+ pending))
        (condition-case condition
            (remember
             (e-mcp-http-list-tools-start
              server
              :on-done #'finish-one
              :on-error #'fail
              :on-event on-event))
          (error
           (fail condition))))
      (when stdio-servers
        (setq pending (1+ pending))
        (condition-case condition
            (remember
             (e-mcp-stdio-refresh-start
              stdio-servers
              :on-done #'finish-one
              :on-error #'fail
              :on-event on-event))
          (error
           (fail condition))))
      (e-tools-request-create
       :cancel (lambda ()
                 (unless settled
                   (setq settled t)
                   (cancel-children))
                 t)
       :metadata (list :transport 'aggregate
                       :kind 'mcp-refresh
                       :server-count (length servers)
                       :cancellable 'cancel-children)))))



(defun e-mcp-client-reset ()
  "Clear remembered catalogs and cancel in-flight catalog discovery."
  (maphash (lambda (_key request)
             (when request
               (e-tools-cancel-request request)))
           e-mcp-client--catalog-starts)
  (setq e-mcp-client--known-servers nil)
  (clrhash e-mcp-client--catalog-cache)
  (clrhash e-mcp-client--catalog-starts)
  t)

(defun e-mcp-client-remember-servers (servers)
  "Remember SERVERS for capability-scoped later activation."
  (dolist (server servers)
    (setf (alist-get (e-mcp-server-id server)
                     e-mcp-client--known-servers nil nil #'equal)
          server))
  e-mcp-client--known-servers)

(defun e-mcp-client-known-server (server-id)
  "Return remembered MCP server SERVER-ID, or nil."
  (alist-get server-id e-mcp-client--known-servers nil nil #'equal))

(defun e-mcp-client-catalog-cache-entry (servers)
  "Return the cached catalog for SERVERS, or the missing sentinel."
  (e-mcp-client--catalog-cache-entry servers))

(defun e-mcp-client-catalog-cached-p (servers)
  "Return non-nil when SERVERS has a cached catalog entry."
  (e-mcp-client--catalog-cached-p servers))

(provide 'e-mcp-client)

;;; e-mcp-client.el ends here
