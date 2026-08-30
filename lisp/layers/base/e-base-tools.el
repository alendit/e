;;; e-base-tools.el --- Base capability composition for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public base-tools composition root.  File/resource/coherence behavior and
;; bash process/stream behavior have separate owners; this file only loads the
;; two semantic components so existing callers retain the stable facade.

;;; Code:

(require 'e-base-tools-file)
(require 'e-base-tools-bash)

(provide 'e-base-tools)

;;; e-base-tools.el ends here
