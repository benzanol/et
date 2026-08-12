;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Redisplay

(et-declare
;; AUTHORING STUB: not yet classified.
(@def set-buffer-redisplay (symbol: Todo newval: Todo op: Todo where: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def line-pixel-height () Todo))

;;; ============================================================
;;; Display properties and measurements

(et-declare
;; AUTHORING STUB: not yet classified.
(@def get-display-property (position: Todo prop: Todo &optional object: Todo properties: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-text-pixel-size (&optional window: Todo from: Todo to: Todo x_limit: Todo y_limit: Todo mode_lines: Todo ignore_line_at_end: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def buffer-text-pixel-size (&optional buffer_or_name: Todo window: Todo x_limit: Todo y_limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def display--line-is-continued-p () Todo))

;;; ============================================================
;;; Tab and tool bars

(et-declare
;; AUTHORING STUB: not yet classified.
(@def tab-bar-height (&optional frame: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def tool-bar-height (&optional frame: Todo pixelwise: Todo) Todo))

;;; ============================================================
;;; Long-line optimizations

(et-declare
;; AUTHORING STUB: not yet classified.
(@def long-line-optimizations-p () Todo))

;;; ============================================================
;;; Redisplay debugging

(et-declare
;; AUTHORING STUB: not yet classified.
(@def dump-glyph-matrix (&optional glyphs: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def dump-frame-glyph-matrix () Todo)
;; AUTHORING STUB: not yet classified.
(@def dump-glyph-row (row: Todo &optional glyphs: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def dump-tab-bar-row (row: Todo &optional glyphs: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def dump-tool-bar-row (row: Todo &optional glyphs: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def trace-redisplay (&optional arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def trace-to-stderr (string: Todo &rest objects: Todo) Todo))

;;; ============================================================
;;; Bidirectional display

(et-declare
;; AUTHORING STUB: not yet classified.
(@def current-bidi-paragraph-direction (&optional buffer: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def bidi-find-overridden-directionality (from: Todo to: Todo object: Todo &optional base_dir: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def move-point-visually (direction: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def bidi-resolved-levels (&optional vpos: Todo) Todo))

;;; ============================================================
;;; Mode line formatting

(et-declare
;; AUTHORING STUB: not yet classified.
(@def format-mode-line (format: Todo &optional face: Todo window: Todo buffer: Todo) Todo))

;;; ============================================================
;;; Text visibility

(et-declare
;; AUTHORING STUB: not yet classified.
(@def invisible-p (pos: Todo) Todo))

;;; ============================================================
;;; Image maps

(et-declare
;; AUTHORING STUB: not yet classified.
(@def lookup-image-map (map: Todo x: Todo y: Todo) Todo))

;;; ============================================================
