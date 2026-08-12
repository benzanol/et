;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Syntax tables

(et-declare
;; AUTHORING STUB: not yet classified.
(@def syntax-table-p (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def syntax-table () Todo)
;; AUTHORING STUB: not yet classified.
(@def standard-syntax-table () Todo)
;; AUTHORING STUB: not yet classified.
(@def copy-syntax-table (&optional table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-syntax-table (table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def char-syntax (character: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def syntax-class-to-char (syntax: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def matching-paren (character: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def string-to-syntax (string: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def modify-syntax-entry (char: Todo newentry: Todo &optional syntax-table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-describe-syntax-value (syntax: Todo) Todo))

;;; ============================================================
;;; Word and character motion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def forward-word (&optional arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def skip-chars-forward (string: Todo &optional lim: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def skip-chars-backward (string: Todo &optional lim: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def skip-syntax-forward (syntax: Todo &optional lim: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def skip-syntax-backward (syntax: Todo &optional lim: Todo) Todo))

;;; ============================================================
;;; Comment motion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def forward-comment (count: Todo) Todo))

;;; ============================================================
;;; Sexp scanning

(et-declare
;; AUTHORING STUB: not yet classified.
(@def scan-lists (from: Todo count: Todo depth: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def scan-sexps (from: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def backward-prefix-chars () Todo)
;; AUTHORING STUB: not yet classified.
(@def parse-partial-sexp (from: Todo to: Todo &optional targetdepth: Todo stopbefore: Todo oldstate: Todo commentstop: Todo) Todo))

;;; ============================================================
