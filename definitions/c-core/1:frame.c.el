;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Frame basics and terminal creation

(et-declare
;; AUTHORING STUB: not yet classified.
(@def framep (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-live-p (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-system (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-windows-min-size (frame: Todo horizontal: Todo ignore: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-terminal-frame (parms: Todo) Todo))

;;; ============================================================
;;; Frame selection and traversal

(et-declare
;; AUTHORING STUB: not yet classified.
(@def select-frame (frame: Todo &optional norecord: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def handle-switch-frame (event: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def selected-frame () Todo)
;; AUTHORING STUB: not yet classified.
(@def old-selected-frame () Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-list () Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-parent (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-ancestor-p (ancestor: Todo descendant: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def next-frame (&optional frame: Todo miniframe: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def previous-frame (&optional frame: Todo miniframe: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def last-nonminibuffer-frame () Todo))

;;; ============================================================
;;; Frame lifecycle, mouse, and focus

(et-declare
;; AUTHORING STUB: not yet classified.
(@def delete-frame (&optional frame: Todo force: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def mouse-position () Todo)
;; AUTHORING STUB: not yet classified.
(@def mouse-pixel-position () Todo)
;; AUTHORING STUB: not yet classified.
(@def set-mouse-position (frame: Todo x: Todo y: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-mouse-pixel-position (frame: Todo x: Todo y: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-frame-visible (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-frame-invisible (&optional frame: Todo force: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def iconify-frame (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-visible-p (frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def visible-frame-list () Todo)
;; AUTHORING STUB: not yet classified.
(@def raise-frame (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def lower-frame (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def redirect-frame-focus (frame: Todo &optional focus-frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-focus (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def x-focus-frame (frame: Todo &optional noactivate: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-after-make-frame (frame: Todo made: Todo) Todo))

;;; ============================================================
;;; Frame parameters and geometry

(et-declare
;; AUTHORING STUB: not yet classified.
(@def frame-parameters (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-parameter (frame: Todo parameter: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def modify-frame-parameters (frame: Todo alist: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-char-height (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-char-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-native-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-native-height (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def tool-bar-pixel-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-text-cols (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-text-lines (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-total-cols (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-total-lines (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-text-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-text-height (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-scroll-bar-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-scroll-bar-height (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-fringe-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-child-frame-border-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-internal-border-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-right-divider-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-bottom-divider-width (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-frame-height (frame: Todo height: Todo &optional pretend: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-frame-width (frame: Todo width: Todo &optional pretend: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-frame-size (frame: Todo width: Todo height: Todo &optional pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-position (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-frame-position (frame: Todo x: Todo y: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-window-state-change (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-frame-window-state-change (&optional frame: Todo arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-scale-factor (&optional frame: Todo) Todo))

;;; ============================================================
;;; X resources

(et-declare
;; AUTHORING STUB: not yet classified.
(@def x-get-resource (attribute: Todo class: Todo &optional component: Todo subclass: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def x-parse-geometry (string: Todo) Todo))

;;; ============================================================
;;; Pointer visibility and fonts

(et-declare
;; AUTHORING STUB: not yet classified.
(@def frame-pointer-visible-p (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame--set-was-invisible (frame: Todo was-invisible: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def reconsider-frame-fonts (frame: Todo) Todo))

;;; ============================================================
