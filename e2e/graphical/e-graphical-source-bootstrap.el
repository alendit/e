;;; e-graphical-source-bootstrap.el --- Prefer checkout source in E2E -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Graphical E2E runs against the checkout, where ignored bytecode can have a
;; newer timestamp after a rebase or source restoration while still describing
;; an older struct layout.  The isolated test process must therefore prefer
;; source by suffix, not merely by mtime.

;;; Code:

(setq e-load-prefer-newer nil
      load-no-native t
      ;; A newer mtime is not proof that ignored bytecode belongs to the
      ;; current checkout after branch switches or source restoration.
      load-prefer-newer nil
      load-suffixes (cons ".el" (delete ".el" load-suffixes)))

(provide 'e-graphical-source-bootstrap)

;;; e-graphical-source-bootstrap.el ends here
