;;; e-mcp-owner-test.el --- Direct MCP owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct owner contracts load the protocol, transport, client, and capability
;; modules without composing the e-mcp facade.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'e-mcp-capability)
(require 'e-mcp-client)
(require 'e-mcp-http)
(require 'e-mcp-protocol)
(require 'e-mcp-stdio)
(require 'e-mcp-transport)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-mcp-owner-test--transport-args (args transport-function)
  "Replace explicit transport option in ARGS with TRANSPORT-FUNCTION."
  (let (result)
    (while args
      (let ((key (pop args))
            (value (pop args)))
        (unless (eq key :transport-function)
          (setq result (append result (list key value))))))
    (append result (list :transport-function transport-function))))

(ert-deftest e-mcp-owner-test-loads-without-facade ()
  "MCP owners are loadable without the facade composition root."
  (e-test-run-fresh-owner-isolation
   "MCP owner isolation"
   '(e-mcp-capability e-mcp-client e-mcp-http e-mcp-protocol
     e-mcp-stdio e-mcp-transport)
   '(e-mcp)
   '((fboundp 'e-mcp-server-create)
     (fboundp 'e-mcp-tool-create)
     (fboundp 'e-mcp-protocol-http-server-p)
     (fboundp 'e-mcp-transport-reserve)
     (fboundp 'e-mcp-list-tools)
     (fboundp 'e-capability-with-mcp-create))))

(ert-deftest e-mcp-owner-test-protocol-classifies-server-values ()
  "Transport classification is a protocol value concern."
  (let ((stdio (e-mcp-server-create :id "stdio" :command '("node" "x")))
        (http (e-mcp-server-create :id "http" :url "http://127.0.0.1")))
    (should-not (e-mcp-protocol-http-server-p stdio))
    (should (e-mcp-protocol-http-server-p http))))

(ert-deftest e-mcp-owner-test-transport-budget-is-explicit-and-bounded ()
  "The shared transport owner admits and releases one request slot."
  (let ((old-limit e-mcp-max-concurrent-requests)
        (old-count e-mcp-transport--active-request-count))
    (unwind-protect
        (progn
          (setq e-mcp-max-concurrent-requests 1)
          (setq e-mcp-transport--active-request-count 0)
          (should (eq (e-mcp-transport-reserve 'owner-test) 'owner-test))
          (should-error (e-mcp-transport-reserve 'owner-test)
                        :type 'e-mcp-backpressure)
          (e-mcp-transport-release 'owner-test)
          (should (= e-mcp-transport--active-request-count 0)))
      (setq e-mcp-max-concurrent-requests old-limit)
      (setq e-mcp-transport--active-request-count old-count))))

(ert-deftest e-mcp-owner-test-client-consumes-a-transport-shaped-result ()
  "Client semantics can use a transport result without HTTP/helper details."
  (let* ((server (e-mcp-server-create :id "stdio" :command '("node" "x")))
         (transport
          (lambda (request)
            (should (equal (plist-get request :op) "list-tools"))
            '(:ok t :result (:tools []))))
         (request-function (symbol-function 'e-mcp-stdio-request)))
    (cl-letf (((symbol-function 'e-mcp-stdio-request)
               (lambda (op servers &rest args)
                 (apply request-function op servers
                        (e-mcp-owner-test--transport-args
                         args transport)))))
      (unwind-protect
          (progn
            (e-mcp-client-reset)
            (should (equal (e-mcp-list-tools (list server)) nil)))
        (e-mcp-client-reset)))))

(provide 'e-mcp-owner-test)

;;; e-mcp-owner-test.el ends here
