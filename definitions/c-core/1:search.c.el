;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Matching

(et-declare
;; AUTHORING STUB: not yet classified.
(@def looking-at (regexp: Todo &optional inhibit-modify: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def posix-looking-at (regexp: Todo &optional inhibit-modify: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def string-match (regexp: Todo string: Todo &optional start: Todo inhibit-modify: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def posix-string-match (regexp: Todo string: Todo &optional start: Todo inhibit-modify: Todo) Todo))

;;; ============================================================
;;; Search commands

(et-declare
;; AUTHORING STUB: not yet classified.
(@def search-backward (string: Todo &optional bound: Todo noerror: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def search-forward (string: Todo &optional bound: Todo noerror: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def re-search-backward (regexp: Todo &optional bound: Todo noerror: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def re-search-forward (regexp: Todo &optional bound: Todo noerror: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def posix-search-backward (regexp: Todo &optional bound: Todo noerror: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def posix-search-forward (regexp: Todo &optional bound: Todo noerror: Todo count: Todo) Todo))

;;; ============================================================
;;; Match replacement and data

(et-declare
;; AUTHORING STUB: not yet classified.
(@def replace-match (newtext: Todo &optional fixedcase: Todo literal: Todo string: Todo subexp: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def match-beginning (subexp: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def match-end (subexp: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def match-data (&optional integers: Todo reuse: Todo reseat: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-match-data (list: Todo &optional reseat: Todo) Todo))

;;; ============================================================
;;; Regexp utilities

(et-declare
;; AUTHORING STUB: not yet classified.
(@def regexp-quote (string: Todo) Todo))

;;; ============================================================
;;; Newline cache

(et-declare
;; AUTHORING STUB: not yet classified.
(@def newline-cache-check (&optional buffer: Todo) Todo))

;;; ============================================================
