;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Text property access

(et-declare
;; AUTHORING STUB: not yet classified.
(@def text-properties-at (position: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-text-property (position: Todo prop: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-char-property (position: Todo prop: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-char-property-and-overlay (position: Todo prop: Todo &optional object: Todo) Todo))

;;; ============================================================
;;; Property changes

(et-declare
;; AUTHORING STUB: not yet classified.
(@def next-char-property-change (position: Todo &optional limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def previous-char-property-change (position: Todo &optional limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def next-single-char-property-change (position: Todo prop: Todo &optional object: Todo limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def previous-single-char-property-change (position: Todo prop: Todo &optional object: Todo limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def next-property-change (position: Todo &optional object: Todo limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def next-single-property-change (position: Todo prop: Todo &optional object: Todo limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def previous-property-change (position: Todo &optional object: Todo limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def previous-single-property-change (position: Todo prop: Todo &optional object: Todo limit: Todo) Todo))

;;; ============================================================
;;; Text property modification

(et-declare
;; AUTHORING STUB: not yet classified.
(@def add-text-properties (start: Todo end: Todo properties: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def put-text-property (start: Todo end: Todo property: Todo value: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-text-properties (start: Todo end: Todo properties: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def add-face-text-property (start: Todo end: Todo face: Todo &optional append: Todo object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def remove-text-properties (start: Todo end: Todo properties: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def remove-list-of-text-properties (start: Todo end: Todo list-of-properties: Todo &optional object: Todo) Todo))

;;; ============================================================
;;; Text property search

(et-declare
;; AUTHORING STUB: not yet classified.
(@def text-property-any (start: Todo end: Todo property: Todo value: Todo &optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def text-property-not-all (start: Todo end: Todo property: Todo value: Todo &optional object: Todo) Todo))

;;; ============================================================
