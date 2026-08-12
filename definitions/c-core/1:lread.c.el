;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Input events

(et-declare
;; AUTHORING STUB: not yet classified.
(@def read-char (&optional prompt: Todo inherit-input-method: Todo seconds: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-event (&optional prompt: Todo inherit-input-method: Todo seconds: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-char-exclusive (&optional prompt: Todo inherit-input-method: Todo seconds: Todo) Todo))

;;; ============================================================
;;; File loading

(et-declare
;; AUTHORING STUB: not yet classified.
(@def get-load-suffixes () Todo)
;; AUTHORING STUB: not yet classified.
(@def load (file: Todo &optional noerror: Todo nomessage: Todo nosuffix: Todo must-suffix: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def locate-file-internal (filename: Todo path: Todo &optional suffixes: Todo predicate: Todo) Todo))

;;; ============================================================
;;; Evaluation and reading

(et-declare
;; AUTHORING STUB: not yet classified.
(@def eval-buffer (&optional buffer: Todo printflag: Todo filename: Todo unibyte: Todo do-allow-print: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def eval-region (start: Todo end: Todo &optional printflag: Todo read-function: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read (&optional stream: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-positioning-symbols (&optional stream: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-from-string (string: Todo &optional start: Todo end: Todo) Todo))

;;; ============================================================
;;; Symbols and obarrays

(et-declare
;; AUTHORING STUB: not yet classified.
(@def intern (string: Todo &optional obarray: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def intern-soft (name: Todo &optional obarray: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def unintern (name: Todo &optional obarray: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def obarray-make (&optional size: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def obarrayp (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def obarray-clear (obarray: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def mapatoms (function: Todo &optional obarray: Todo) Todo))

;;; ============================================================
