;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Coding system predicates and input

(et-declare
;; AUTHORING STUB: not yet classified.
(@def coding-system-p (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-non-nil-coding-system (prompt: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-coding-system (prompt: Todo &optional default_coding_system: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def check-coding-system (coding_system: Todo) Todo))

;;; ============================================================
;;; Coding system detection

(et-declare
;; AUTHORING STUB: not yet classified.
(@def detect-coding-region (start: Todo end: Todo &optional highest: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def detect-coding-string (string: Todo &optional highest: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def find-coding-systems-region-internal (start: Todo end: Todo &optional exclude: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def unencodable-char-position (start: Todo end: Todo coding_system: Todo &optional count: Todo string: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def check-coding-systems-region (start: Todo end: Todo coding_system_list: Todo) Todo))

;;; ============================================================
;;; Coding conversion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def decode-coding-region (start: Todo end: Todo coding_system: Todo &optional destination: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def encode-coding-region (start: Todo end: Todo coding_system: Todo &optional destination: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-encode-string-utf-8 (string: Todo buffer: Todo nocopy: Todo handle_8_bit: Todo handle_over_uni: Todo encode_method: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-decode-string-utf-8 (string: Todo buffer: Todo nocopy: Todo handle_8_bit: Todo handle_over_uni: Todo decode_method: Todo count: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def decode-coding-string (string: Todo coding_system: Todo &optional nocopy: Todo buffer: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def encode-coding-string (string: Todo coding_system: Todo &optional nocopy: Todo buffer: Todo) Todo))

;;; ============================================================
;;; Character coding conversion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def decode-sjis-char (code: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def encode-sjis-char (ch: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def decode-big5-char (code: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def encode-big5-char (ch: Todo) Todo))

;;; ============================================================
;;; Terminal coding systems

(et-declare
;; AUTHORING STUB: not yet classified.
(@def set-terminal-coding-system-internal (coding_system: Todo &optional terminal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-safe-terminal-coding-system-internal (coding_system: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def terminal-coding-system (&optional terminal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-keyboard-coding-system-internal (coding_system: Todo &optional terminal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def keyboard-coding-system (&optional terminal: Todo) Todo))

;;; ============================================================
;;; Operation coding systems

(et-declare
;; AUTHORING STUB: not yet classified.
(@def find-operation-coding-system (operation: Todo &rest args: Todo) Todo))

;;; ============================================================
;;; Coding system priorities

(et-declare
;; AUTHORING STUB: not yet classified.
(@def set-coding-system-priority (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coding-system-priority-list (&optional highestp: Todo) Todo))

;;; ============================================================
;;; Coding system definitions

(et-declare
;; AUTHORING STUB: not yet classified.
(@def define-coding-system-internal (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coding-system-put (coding_system: Todo prop: Todo val: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def define-coding-system-alias (alias: Todo coding_system: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coding-system-base (coding_system: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coding-system-plist (coding_system: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coding-system-aliases (coding_system: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coding-system-eol-type (coding_system: Todo) Todo))

;;; ============================================================
