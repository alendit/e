;;; e-board-observation.el --- Board-owned participant activity observation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The Board observation seam is the consumer-shaped contract shared by future
;; shells and agent actions.  It delegates one detached bounded page to the
;; SQLite application service.  It has no knowledge of subagent runners,
;; process-local handles, or live execution registries.

;;; Code:

(require 'e-board-sqlite-service)

(defun e-board-observation-activity-page-start
    (target &rest arguments)
  "Return work for TARGET's bounded participant/activity page.
ARGUMENTS are keyword arguments accepted by
`e-board-sqlite-publication-target-activity-page-start'.  The returned page is
detached and request-owned; this observation service retains no page after the
consumer work handle settles."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (apply #'e-board-sqlite-publication-target-activity-page-start
         target arguments))

(provide 'e-board-observation)

;;; e-board-observation.el ends here
