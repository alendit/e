;;; e-board-storage-sqlite.el --- SQLite Board storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Maps the Board-owned storage port onto the existing typed runtime store.
;; SQL, schema, revisions, and composite transaction details remain worker-side.

;;; Code:

(require 'e-board-storage)
(require 'e-runtime-store)
(require 'e-runtime-store-codec)

(defun e-board-storage-sqlite--stable-id (prefix value)
  "Return a stable command identity for PREFIX and causal VALUE."
  (format "%s:%s" prefix
          (secure-hash 'sha256 (e-runtime-store-codec-encode value))))

(defun e-board-storage-sqlite--call (runtime operation arguments)
  "Dispatch OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('create-board
     (pcase-let ((`(,board-id ,principal ,root) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-create :board-id board-id
              :trusted-principal principal :root root))))
    ('board
     (e-runtime-store-call runtime 'read
                           (list :op 'board-get :board-id (car arguments))))
    ('list-boards
     (pcase-let ((`(,after ,limit) arguments))
       (e-runtime-store-call runtime 'read
                             (list :op 'board-list :after after :limit limit))))
    ('clear-board
     (pcase-let ((`(,board-id ,expected) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-clear :board-id board-id
              :expected-revision expected))))
    ('publish-record
     (pcase-let ((`(,board-id ,generation ,expected ,record ,source) arguments))
       (when source
         (setq source
               (list :kind (plist-get source :kind)
                     :key (plist-get source :key)
                     :hash
                     (e-board-storage-signature-hash
                      (plist-get source :signature)))))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-record-put :board-id board-id :generation generation
              :expected-revision expected :record record :source source))))
    ('record-page
     (pcase-let ((`(,board-id ,generation ,after ,limit ,selector) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-record-page :board-id board-id :generation generation
              :after after :limit limit :selector selector))))
    ('commit-routing
     (pcase-let
         ((`(,board-id ,generation ,expected ,message-id ,outcome ,pickups)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-routing-put :board-id board-id :generation generation
              :expected-revision expected :message-id message-id
              :outcome outcome :pickups (vconcat pickups)))))
    ('routing
     (pcase-let ((`(,board-id ,generation ,message-id) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-routing-get :board-id board-id
              :generation generation :message-id message-id))))
    ('transition-pickup
     (pcase-let
         ((`(,board-id ,generation ,delivery-id ,expected ,transition ,data)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-pickup-transition :board-id board-id
              :generation generation :delivery-id delivery-id
              :expected-pickup-revision expected
              :transition transition :data data))))
    ('unresolved-pickups
     (pcase-let ((`(,board-id ,generation ,participant-id ,limit) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-pickup-list :board-id board-id :generation generation
              :participant-id participant-id :limit limit))))
    ('put-participant
     (pcase-let ((`(,board-id ,generation ,expected ,participant) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-participant-put :board-id board-id
              :generation generation :expected-revision expected
              :participant participant))))
    ('delete-participant
     (pcase-let ((`(,board-id ,generation ,expected ,participant-id) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-participant-delete :board-id board-id
              :generation generation :expected-revision expected
              :participant-id participant-id))))
    ('publish-participant
     (pcase-let ((`(,board-id ,generation ,expected ,participant-id) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-participant-publish :board-id board-id
              :generation generation :expected-revision expected
              :participant-id participant-id))))
    ('participants
     (pcase-let ((`(,board-id ,generation ,limit) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-participant-list :board-id board-id
              :generation generation :limit limit))))
    ('put-replay-progress
     (pcase-let
         ((`(,board-id ,generation ,subscription-id ,position ,expected)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'board-replay-progress-put :board-id board-id
              :generation generation :subscription-id subscription-id
              :position position :expected-revision expected))))
    ('replay-progress
     (pcase-let ((`(,board-id ,generation ,subscription-id) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'board-replay-progress-get :board-id board-id
              :generation generation :subscription-id subscription-id))))
    ('admit-pickup
     (pcase-let
         ((`(,board-id ,generation ,delivery-id ,pickup-revision ,session-id
                      ,session-revision ,record ,lane)
           arguments))
       (let* ((identity (list board-id generation delivery-id session-id))
              (body
               (list :op 'board-pickup-session-admit :board-id board-id
                     :generation generation :delivery-id delivery-id
                     :expected-pickup-revision pickup-revision
                     :session-id session-id
                     :expected-session-revision session-revision
                     :record record :lane lane)))
         (e-runtime-store-call
          runtime 'write body
          (e-board-storage-sqlite--stable-id "board-pickup-admission" identity)))))
    ('status
     (append (list :backend 'sqlite)
             (e-runtime-store-status runtime)))
    (_ (signal 'e-board-storage-error
               (list "Unknown Board storage operation" operation)))))

(defun e-board-storage-sqlite-create (runtime)
  "Return a Board storage port backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-board-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-board-storage-sqlite--call runtime operation arguments))))

(provide 'e-board-storage-sqlite)

;;; e-board-storage-sqlite.el ends here
