;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Charset definition and mapping

(et-declare
 ;; AUTHORING STUB: not yet classified.
 (@def charsetp (object: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def map-charset-chars (function: Todo charset: Todo &optional arg: Todo from-code: Todo to-code: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def define-charset-internal (&rest args: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def define-charset-alias (alias: Todo charset: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def charset-plist (charset: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def set-charset-plist (charset: Todo plist: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def unify-charset (charset: Todo &optional unify-map: Todo deunify: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def get-unused-iso-final-char (dimension: Todo chars: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def declare-equiv-charset (dimension: Todo chars: Todo final-char: Todo charset: Todo) Todo))

;;; ============================================================
;;; Charset discovery and conversion

(et-declare
 ;; AUTHORING STUB: not yet classified.
 (@def find-charset-region (beg: Todo end: Todo &optional table: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def find-charset-string (str: Todo &optional table: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def decode-char (charset: Todo code-point: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def encode-char (ch: Todo charset: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def make-char (charset: Todo &optional code1: Todo code2: Todo code3: Todo code4: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def split-char (ch: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def char-charset (ch: Todo &optional restriction: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def charset-after (&optional pos: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def iso-charset (dimension: Todo chars: Todo final-char: Todo) Todo))

;;; ============================================================
;;; Charset maps and priority

(et-declare
 ;; AUTHORING STUB: not yet classified.
 (@def clear-charset-maps () Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def charset-priority-list (&optional highestp: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def set-charset-priority (&rest charsets: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def charset-id-internal (&optional charset: Todo) Todo)
 ;; AUTHORING STUB: not yet classified.
 (@def sort-charsets (charsets: Todo) Todo))

;;; ============================================================
