;;; e-board-runtime-test-support.el --- Shared board runtime test fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;; Shared fixture and ingress adapters for focused board-runtime suites.

;;; Code:

(require 'ert)
(require 'e-board-runtime)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-task-queue)

(defconst e-board-runtime-test--production-post-input
  (symbol-function 'e-board-runtime-post-input)
  "Unwrapped production ingress used by the runtime test adapter.")

(defconst e-board-runtime-test--core-post-input
  (symbol-function 'e-board-post-input)
  "Unwrapped core ingress used by the runtime test adapter.")

(defun e-board-runtime-test--source-post-input (source-board &rest arguments)
  "Post through SOURCE-BOARD with an authenticated registry test actor."
  (if (plist-member arguments :requester-actor)
      (apply e-board-runtime-test--core-post-input source-board arguments)
    (let* ((board (e-board-registry-get (e-board-id source-board)))
           (id "runtime-source-test-client")
           (client (or (gethash id (e-board-registry-board-clients board))
                       (e-board-registry-attach-client
                        board :id id
                        :principal (e-board-registry-board-principal board))))
           (actor (list 'client id (e-board-registry-client-generation client))))
      (apply e-board-runtime-test--core-post-input
             source-board (append arguments (list :requester-actor actor))))))

(defun e-board-runtime-test--post-input (board &rest arguments)
  "Call production ingress, supplying a generation-fenced test client.
Tests that explicitly provide `:requester' retain that exact requester."
  (if (or (plist-member arguments :requester)
          (not e-board-runtime--admission-open-p))
      (apply e-board-runtime-test--production-post-input board arguments)
    (let* ((id "runtime-test-client")
           (clients (e-board-registry-board-clients board))
           (client (or (gethash id clients)
                       (e-board-registry-attach-client
                        board :id id
                        :principal (e-board-registry-board-principal board))))
           (requester
            (e-board-registry-client-requester-context
             board (e-board-registry-client-id client))))
      (apply e-board-runtime-test--production-post-input
             board (append arguments (list :requester requester))))))

(defmacro e-board-runtime-test--with-empty-state (&rest body)
  "Run BODY with isolated board, registry, and runtime attachment state."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'e-board-runtime-post-input)
              #'e-board-runtime-test--post-input)
             ((symbol-function 'e-board-post-input)
              #'e-board-runtime-test--source-post-input))
     (let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
          (e-board-registry--boards (make-hash-table :test 'equal))
          (e-board-registry--id-sequence 0)
          (e-board-runtime--attachments (make-hash-table :test 'equal))
          (e-board-runtime--session-attachments (make-hash-table :test 'equal))
          (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
          (e-board-runtime--invocations (make-hash-table :test 'equal))
          (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
          (e-board-runtime--producer-inputs (make-hash-table :test 'equal))
          (e-board-runtime--producer-deliveries (make-hash-table :test 'equal))
          (e-board-runtime--producer-turns (make-hash-table :test 'equal))
          (e-board-runtime--producer-epoch 0)
          (e-board-runtime--producer-head nil)
          (e-board-runtime--producer-tail nil)
          (e-board-runtime--producer-drain-scheduled nil)
          (e-board-runtime--producer-scheduler nil)
          (e-board-runtime--admission-open-p t)
          (e-board-runtime--admission-epoch 0)
          (e-board-runtime--quiescence-current nil)
          (e-board-runtime--unsettled-control-count 0)
          (e-board-runtime--unsettled-invocation-count 0)
          (e-board-runtime--unsettled-deferred-hook-count 0)
          (e-board-runtime--unsettled-producer-count 0)
          (e-board-runtime--unsettled-generation 0)
          (e-board-runtime--unsettled-change-function nil)
          (e-board-runtime--unsettled-change-functions nil)
          (e-board-registry--unsettled-pickup-count 0)
          (e-board-registry--unsettled-effect-count 0)
          (e-board-registry--unsettled-routing-count 0)
          (e-board-registry--unsettled-generation 0)
          (e-board-registry--unsettled-change-function nil)
          (e-board-registry--unsettled-change-functions nil)
          (e-harness-aggregate-unsettled-change-hook nil)
          (e-work--unsettled-count 0)
          (e-work--unsettled-generation 0)
          (e-work--unsettled-change-functions nil)
          (e-task-queue--unsettled-write-count 0)
          (e-task-queue--failed-write-count 0)
          (e-task-queue--unsettled-generation 0)
          (e-task-queue--unsettled-change-functions nil)
          (e-board-runtime--control-sequence 0)
          (e-board-runtime--deferred-hook-head nil)
          (e-board-runtime--deferred-hook-tail nil)
          (e-board-runtime--deferred-hook-drain-scheduled nil)
          (e-board-runtime--deferred-hook-generation 0)
          (e-board-runtime--work-activity-mailboxes (make-hash-table :test 'equal))
          (e-board-runtime--pending-activity-head nil)
          (e-board-runtime--pending-activity-tail nil)
          (e-board-runtime--pending-activity-set (make-hash-table :test 'equal))
          (e-board-runtime--activity-drain-scheduled nil)
          (e-board-runtime--activity-drain-generation 0)
          (e-board-runtime--pending-pickup-head nil)
          (e-board-runtime--pending-pickup-tail nil)
          (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
          (e-board-runtime--pickup-drain-scheduled nil)
          (e-board-runtime--pickup-drain-generation 0)
          (e-harness-registry--instances (make-hash-table :test 'equal))
          (e-harness-registry--factories (make-hash-table :test 'equal))
          (e-harness-registry--generations (make-hash-table :test 'equal))
          (e-harness-registry--invalidation-events (make-hash-table :test 'equal))
          (e-harness-instance--instances (make-hash-table :test 'equal))
          (e-harness-instance--defaults (make-hash-table :test 'equal))
          (e-harness-instance--session-stores (make-hash-table :test 'equal))
          (e-harness-instance--generation 0))
       (e-harness-turn-state-reset-aggregate)
       (unwind-protect
           (progn ,@body)
         (e-harness-turn-state-reset-aggregate)))))
