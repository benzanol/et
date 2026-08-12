;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Utilities

(et-declare
;; AUTHORING STUB: not yet classified.
(@def dump-colors () Todo))

;;; ============================================================
;;; Frames and faces

(et-declare
;; AUTHORING STUB: not yet classified.
(@def clear-face-cache (&optional thoroughly: Todo) Todo))

;;; ============================================================
;;; X pixmaps

(et-declare
;; AUTHORING STUB: not yet classified.
(@def bitmap-spec-p (object: Todo) Todo))

;;; ============================================================
;;; Color handling

(et-declare
;; AUTHORING STUB: not yet classified.
(@def color-values-from-color-spec (spec: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def color-gray-p (color: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def color-supported-p (color: Todo &optional frame: Todo background-p: Todo) Todo))

;;; ============================================================
;;; XLFD font names

(et-declare
;; AUTHORING STUB: not yet classified.
(@def x-family-fonts (&optional family: Todo frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def x-list-fonts (pattern: Todo &optional face: Todo frame: Todo maximum: Todo width: Todo) Todo))

;;; ============================================================
;;; Lisp faces

(et-declare
;; AUTHORING STUB: not yet classified.
(@def internal-make-lisp-face (face: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-lisp-face-p (face: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-copy-lisp-face (from: Todo to: Todo frame: Todo new-frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-set-lisp-face-attribute (face: Todo attr: Todo value: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-face-x-get-resource (resource: Todo class: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-set-lisp-face-attribute-from-resource (face: Todo attr: Todo value: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def face-attribute-relative-p (attribute: Todo value: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def merge-face-attribute (attribute: Todo value1: Todo value2: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-get-lisp-face-attribute (symbol: Todo keyword: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-lisp-face-attribute-values (attr: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-merge-in-global-face (face: Todo frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def face-font (face: Todo &optional frame: Todo character: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-lisp-face-equal-p (face1: Todo face2: Todo &optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-lisp-face-empty-p (face: Todo &optional frame: Todo) Todo))

;;; ============================================================
;;; Realized faces

(et-declare
;; AUTHORING STUB: not yet classified.
(@def color-distance (color1: Todo color2: Todo &optional frame: Todo metric: Todo) Todo))

;;; ============================================================
;;; Face cache

(et-declare
;; AUTHORING STUB: not yet classified.
(@def face-attributes-as-vector (plist: Todo) Todo))

;;; ============================================================
;;; Face capability testing

(et-declare
;; AUTHORING STUB: not yet classified.
(@def display-supports-face-attributes-p (attributes: Todo &optional display: Todo) Todo))

;;; ============================================================
;;; Font selection

(et-declare
;; AUTHORING STUB: not yet classified.
(@def internal-set-font-selection-order (order: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-set-alternative-font-family-alist (alist: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-set-alternative-font-registry-alist (alist: Todo) Todo))

;;; ============================================================
;;; Face realization

(et-declare
;; AUTHORING STUB: not yet classified.
(@def tty-suppress-bold-inverse-default-colors (suppress: Todo) Todo))

;;; ============================================================
;;; Computing faces

(et-declare
;; AUTHORING STUB: not yet classified.
(@def x-load-color-file (filename: Todo) Todo))

;;; ============================================================
;;; Tests

(et-declare
;; AUTHORING STUB: not yet classified.
(@def dump-face (&optional n: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def show-face-resources () Todo))

;;; ============================================================
