;;; e-session-query-command.el --- Relational session command derivation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure command interpretation for the SQLite-authoritative session path.
;; Callers supply one detached current query row and a sealed session command;
;; this module returns one canonical journal record, its complete next query
;; row, and the bounded public result.  It never reads storage or installs a
;; session aggregate in Emacs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-session-aggregate)
(require 'e-session-query)

(define-error 'e-session-query-command-error
  "Invalid relational session command"
  'e-session-error)

(defconst e-session-query-command--wire-keys
  '(:tag :session-id :arguments :request-id :delta-id :timestamp)
  "Exact fields carried by one sealed relational command.")

(defun e-session-query-command-to-wire (command)
  "Return a detached protocol value for sealed COMMAND."
  (unless (e-session-aggregate-command-p command)
    (signal 'wrong-type-argument
            (list 'e-session-aggregate-command-p command)))
  (let* ((tag (e-session-aggregate-command-tag command))
         (arguments
          (copy-tree (e-session-aggregate-command-arguments command) t)))
    ;; Runtime structs are useful request-local values but are not protocol
    ;; data.  Normalize the one command field whose public facade accepts a
    ;; struct before handing the sealed command to the storage adapter.
    (when (and (eq tag 'context-generation)
               (e-context-lifetime-generation-p
                (plist-get arguments :generation)))
      (setq arguments
            (plist-put arguments :generation
                       (e-context-lifetime-generation-record
                        (plist-get arguments :generation)))))
    (list :tag tag
          :session-id
          (copy-sequence (e-session-aggregate-command-session-id command))
          :arguments arguments
          :request-id
          (copy-sequence (e-session-aggregate-command-request-id command))
          :delta-id
          (copy-sequence (e-session-aggregate-command-delta-id command))
          :timestamp
          (copy-sequence (e-session-aggregate-command-timestamp command)))))

(defun e-session-query-command-from-wire (value)
  "Validate VALUE and return its sealed relational command."
  (unless (and (proper-list-p value)
               (= (length value) (* 2 (length e-session-query-command--wire-keys)))
               (let ((tail value) keys valid)
                 (setq valid t)
                 (while (and valid tail)
                   (let ((key (pop tail)))
                     (pop tail)
                     (if (or (not (memq key e-session-query-command--wire-keys))
                             (memq key keys))
                         (setq valid nil)
                       (push key keys))))
                 (and valid
                      (= (length keys)
                         (length e-session-query-command--wire-keys)))))
    (signal 'e-session-query-command-error
            (list "Malformed relational session command" value)))
  (let ((tag (plist-get value :tag))
        (session-id (plist-get value :session-id))
        (arguments (plist-get value :arguments))
        (request-id (plist-get value :request-id))
        (delta-id (plist-get value :delta-id))
        (timestamp (plist-get value :timestamp)))
    (e-session-aggregate-command-practical-preflight arguments)
    (e-session-aggregate-command-validate tag session-id arguments)
    (unless (and (stringp request-id) (not (string-empty-p request-id))
                 (stringp delta-id) (not (string-empty-p delta-id))
                 (stringp timestamp) (not (string-empty-p timestamp)))
      (signal 'e-session-query-command-error
              (list "Malformed relational command identity" value)))
    (e-session-aggregate-command--create
     :tag tag :session-id (copy-sequence session-id)
     :arguments (e-session-aggregate-command-freeze arguments)
     :request-id (copy-sequence request-id)
     :delta-id (copy-sequence delta-id)
     :timestamp (copy-sequence timestamp))))

(defun e-session-query-command--entry
    (state type fields command-id timestamp &optional explicit-id)
  "Return a detached TYPE entry derived from STATE and bounded FIELDS."
  (let ((entry (copy-tree fields t)))
    (plist-put entry :type type)
    (plist-put entry :id (or explicit-id (plist-get entry :id) command-id))
    (unless (plist-member entry :parent-id)
      (plist-put entry :parent-id (plist-get state :current-head-id)))
    (unless (plist-member entry :created-at)
      (plist-put entry :created-at timestamp))
    entry))

(defun e-session-query-command--positioned-record (record position)
  "Return a detached derivation copy of RECORD at journal POSITION."
  (let ((copy (copy-tree record t)))
    (plist-put copy :journal-position position)
    copy))

(defun e-session-query-command-interpret (state command)
  "Interpret sealed COMMAND against detached current query STATE.

The supported command set is the SQLite-authoritative session vocabulary.
The returned plist contains `:record', `:query-delta', and `:result'."
  (unless (e-session-aggregate-command-p command)
    (signal 'wrong-type-argument
            (list 'e-session-aggregate-command-p command)))
  (let* ((session-id (e-session-aggregate-command-session-id command))
         (arguments (e-session-aggregate-command-arguments command))
         (tag (e-session-aggregate-command-tag command))
         (request-id (e-session-aggregate-command-request-id command))
         (delta-id (e-session-aggregate-command-delta-id command))
         (timestamp (e-session-aggregate-command-timestamp command))
         (next-position (if state
                            (1+ (plist-get state :journal-position))
                          1))
         record result)
    (unless (or (and (eq tag 'create) (null state))
                (and state
                     (equal session-id (plist-get state :session-id))))
      (signal 'e-session-query-command-error
              (list "Command owner does not match query state"
                    session-id (and state (plist-get state :session-id)))))
    (when state (e-session-query-state-validate state))
    (pcase tag
      ('create
       (let ((metadata
              (e-session-metadata-validate
               (e-session-metadata-normalize-for-replay
                (plist-get arguments :metadata)))))
         (setq record
               (list :type "session" :session-id session-id
                     :id delta-id :request-id request-id :delta-id delta-id
                     :timestamp timestamp :created-at timestamp
                     :updated-at timestamp :metadata metadata
                     :name (plist-get metadata :name) :turn-options nil
                     :current-branch nil :board-output-sequence 0
                     :board-activity-sequence 0)
               result (list :id session-id :metadata (copy-tree metadata t)))))
      ('append-message
       (let* ((message
               (e-session-aggregate--message-with-created-at
                (plist-get arguments :message) timestamp))
              (entry
               (e-session-query-command--entry
                state 'message message delta-id timestamp)))
         (when (and (eq (plist-get entry :role) 'assistant)
                    (not (plist-member entry :board-output-sequence)))
           (plist-put entry :board-output-sequence
                      (1+ (plist-get state :board-output-sequence))))
         (setq record
               (list :type "message" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :timestamp (plist-get entry :created-at)
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :message entry)
               result (copy-tree entry t))))
      ((or 'append-activity 'context-curation-response)
       (let* ((curation-p (eq tag 'context-curation-response))
              (explicit-id (and curation-p
                                (plist-get arguments :response-entry-id)))
              (event-type (if curation-p 'context-curation-response
                            (plist-get arguments :event-type)))
              (payload (if curation-p
                           (list :response-entry-id explicit-id)
                         (plist-get arguments :payload)))
              (entry
               (e-session-query-command--entry
                state 'activity-event
                (append
                 (when (and (not curation-p)
                            (plist-get arguments :checkpoint-retain))
                   (list :checkpoint-retain t))
                 (list :turn-id (plist-get arguments :turn-id)
                       :event-type event-type :payload payload))
                delta-id timestamp explicit-id))
              (sequence
               (1+ (plist-get state :board-activity-sequence))))
         (plist-put entry :board-activity-sequence sequence)
         (setq record
               (append
                (list :type "activity-event" :session-id session-id
                      :request-id request-id :delta-id delta-id
                      :id (plist-get entry :id)
                      :parent-id (plist-get entry :parent-id)
                      :turn-id (plist-get entry :turn-id)
                      :board-activity-sequence sequence
                      :timestamp (plist-get entry :created-at)
                      :event-type event-type :payload payload)
                (when (plist-get entry :checkpoint-retain)
                  (list :checkpoint-retain t)))
               result (copy-tree entry t))))
      ('message-display
       ;; The consumer supplies an identity from its detached visible page.
       ;; SQLite remains authoritative; an obsolete identity produces a
       ;; harmless journal disposition rather than forcing aggregate replay.
       (let ((message-id (plist-get arguments :message-id)))
         (setq record
               (list :type "message-display" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :timestamp timestamp :id message-id
                     :display
                     (when-let* ((display (plist-get arguments :display)))
                       (symbol-name display)))
               result (list :id message-id
                            :display (plist-get arguments :display)))))
      ('process-report
       (let ((entry
              (e-session-query-command--entry
               state 'process-report (plist-get arguments :report)
               delta-id timestamp)))
         (setq record
               (list :type "process-report" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :timestamp timestamp :report entry)
               result (copy-tree entry t))))
      ('branch-summary
       (let ((entry
              (e-session-query-command--entry
               state 'branch-summary
               (list :branch-id (plist-get arguments :branch-id)
                     :summary (plist-get arguments :summary)
                     :metadata (plist-get arguments :metadata))
               delta-id timestamp)))
         (setq record
               (list :type "branch-summary" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :timestamp timestamp
                     :branch-id (plist-get entry :branch-id)
                     :summary (plist-get entry :summary)
                     :metadata (plist-get entry :metadata))
               result (copy-tree entry t))))
      ('compaction
       (let ((entry
              (e-session-query-command--entry
               state 'compaction
               (list :summary (plist-get arguments :summary)
                     :branch-id (plist-get arguments :branch-id)
                     :range (plist-get arguments :range)
                     :first-kept-entry-id
                     (plist-get arguments :first-kept-entry-id)
                     :tokens-before (plist-get arguments :tokens-before)
                     :tokens-kept (plist-get arguments :tokens-kept)
                     :metadata (plist-get arguments :metadata))
               delta-id timestamp)))
         (setq record
               (append
                (list :type "compaction" :session-id session-id
                      :request-id request-id :delta-id delta-id
                      :id (plist-get entry :id)
                      :parent-id (plist-get entry :parent-id)
                      :timestamp timestamp)
                (cl-loop for key in '(:summary :branch-id :range
                                      :first-kept-entry-id :tokens-before
                                      :tokens-kept :metadata)
                         append (list key (plist-get entry key))))
               result (copy-tree entry t))))
      ('provider-anchor
       (let ((entry
              (e-session-query-command--entry
               state 'provider-anchor
               (list :provider-id (plist-get arguments :provider-id)
                     :model (plist-get arguments :model)
                     :covered-entry-id (plist-get arguments :covered-entry-id)
                     :fingerprints (plist-get arguments :fingerprints)
                     :metadata (plist-get arguments :metadata))
               delta-id timestamp)))
         (setq record
               (append
                (list :type "provider-anchor" :session-id session-id
                      :request-id request-id :delta-id delta-id
                      :id (plist-get entry :id)
                      :parent-id (plist-get entry :parent-id)
                      :timestamp timestamp)
                (cl-loop for key in '(:provider-id :model :covered-entry-id
                                      :fingerprints :metadata)
                         append (list key (plist-get entry key))))
               result (copy-tree entry t))))
      ('context-generation
       (let* ((raw (plist-get arguments :generation))
              (context-record
               (if (e-context-lifetime-generation-p raw)
                   (e-context-lifetime-generation-record raw)
                 (let ((normalized
                        (e-session-aggregate--normalize-context-record
                         'context-generation raw)))
                   (unless (equal normalized raw)
                     (signal 'e-session-query-command-error
                             (list "Context generation is not canonical")))
                   raw)))
              (entry
               (e-session-query-command--entry
                state 'context-generation
                (list :context-record context-record)
                delta-id timestamp)))
         (setq record
               (list :type "context-generation" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :timestamp timestamp :context-record context-record)
               result (copy-tree entry t))))
      ('context-curation-package
       (let* ((package
               (e-session-aggregate--validate-owned-context-curation-package
                (plist-get arguments :package)))
              (active-generation-id
               (plist-get state :current-context-generation-id)))
         (dolist (component (list (plist-get package :promotion)
                                  (plist-get package :erasure)))
           (when (and component
                      (not (equal (plist-get component :generation-id)
                                  active-generation-id)))
             (signal
              'e-session-query-command-error
              (list "Context curation has no active generation owner"
                    session-id (plist-get component :generation-id)
                    active-generation-id))))
         (let* ((package-id
                 (e-session-aggregate--context-curation-package-id
                  session-id package))
                (entry
                 (e-session-query-command--entry
                  state 'context-curation-package
                  (list :promotion (plist-get package :promotion)
                        :erasure (plist-get package :erasure))
                  delta-id timestamp package-id)))
           (setq record
                 (append
                  (e-session-aggregate--context-curation-package-record
                   session-id package package-id
                   (plist-get state :current-head-id) timestamp)
                  (list :request-id request-id :delta-id delta-id))
                 result (copy-tree entry t)))))
      ('clear-messages
       (setq record
             (list :type "messages-cleared" :session-id session-id
                   :request-id request-id :delta-id delta-id :id delta-id
                   :parent-id (plist-get state :root-event-id)
                   :timestamp timestamp)
             result (list :id delta-id :type 'messages-cleared)))
      ('board-messages-clear
       (setq record
             (list :type "board-messages-cleared" :session-id session-id
                   :request-id request-id :delta-id delta-id :id delta-id
                   :timestamp timestamp)
             result nil))
      ('board-state
       (let ((association
              (append
               (list :board-id (plist-get arguments :board-id)
                     :principal (plist-get arguments :principal))
               (when-let* ((role (plist-get arguments :association-role)))
                 (list :association-role role))
               (when-let* ((policy (plist-get arguments :routing-policy)))
                 (list :routing-policy
                       (e-session-board-routing-policy-normalize-owned
                        policy))))))
         (setq record
               (list :type "board-session-state" :session-id session-id
                     :request-id request-id :delta-id delta-id :id delta-id
                     :timestamp timestamp :board-state association
                     :board-id (plist-get association :board-id)
                     :principal (plist-get association :principal)
                     :board-output-sequence
                     (plist-get state :board-output-sequence)
                     :board-activity-sequence
                     (plist-get state :board-activity-sequence))
               result (copy-tree association t))))
      ('delete
       (setq record
             (list :type "session-deleted" :session-id session-id
                   :request-id request-id :delta-id delta-id :id delta-id
                   :timestamp timestamp)
             result t))
      ('session-info
       (let* ((field (plist-get arguments :field))
              (value (plist-get arguments :value))
              (parent-id (plist-get state :current-head-id)))
         (setq record
               (if (eq field 'current-branch)
                   (list :type "current-branch" :session-id session-id
                         :id delta-id :request-id request-id
                         :delta-id delta-id :parent-id parent-id
                         :timestamp timestamp :branch-id value)
                 (append
                  (list :type "session-info" :session-id session-id
                        :id delta-id :request-id request-id
                        :delta-id delta-id :parent-id parent-id
                        :timestamp timestamp :field field :value value)
                  (pcase field
                    ('context-references
                     (list :owner (plist-get arguments :owner)))
                    ('context-reference
                     (list :key (plist-get arguments :key)))
                    ('capability-state
                     (list :capability-id
                           (plist-get arguments :capability-id)
                           :version (plist-get arguments :version)))))))))
      (_
       (signal 'e-session-query-command-error
               (list "Unsupported relational session command" tag))))
    (let* ((derivation-record
            (e-session-query-command--positioned-record record
                                                        (if state next-position 1)))
           (query-delta
            (e-session-query-state-apply-record state derivation-record)))
      (when (eq tag 'session-info)
        (let ((field (plist-get arguments :field)))
          (setq result
                (pcase field
                  ((or 'metadata 'context-references)
                   (copy-tree (plist-get arguments :value) t))
                  ('capability-state
                   (let ((value (plist-get arguments :value))
                         (version (plist-get arguments :version)))
                     (copy-tree
                      (if version (list :version version :state value) value)
                      t)))
                  ((or 'config 'context-reference)
                   (copy-tree (plist-get query-delta :metadata) t))
                  ('turn-options
                   (copy-tree (plist-get query-delta :turn-options) t))
                  ('current-branch
                   (copy-tree (plist-get query-delta :current-branch) t))
                  ('name (copy-tree query-delta t))))))
      (list :record record :query-delta query-delta :result result))))

(provide 'e-session-query-command)

;;; e-session-query-command.el ends here
