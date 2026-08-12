;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Column and indentation

(et-declare
;; AUTHORING STUB: not yet classified.
(@def current-column () Todo)
;; AUTHORING STUB: not yet classified.
(@def indent-to (column: Todo &optional minimum: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def current-indentation () Todo)
;; AUTHORING STUB: not yet classified.
(@def move-to-column (column: Todo &optional force: Todo) Todo))

;;; ============================================================
;;; Screen motion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def compute-motion (from: Todo frompos: Todo to: Todo topos: Todo width: Todo offsets: Todo window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def line-number-display-width (&optional pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def vertical-motion (lines: Todo &optional window: Todo cur_col: Todo) Todo))

;;; ============================================================
