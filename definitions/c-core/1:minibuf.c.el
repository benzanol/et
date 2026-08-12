;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Minibuffer state

(et-declare
;; AUTHORING STUB: not yet classified.
(@def active-minibuffer-window () Todo)
;; AUTHORING STUB: not yet classified.
(@def set-minibuffer-window (window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def minibufferp (&optional buffer: Todo live: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def innermost-minibuffer-p (&optional buffer: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-innermost-command-loop-p (&optional buffer: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def abort-minibuffers () Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-prompt-end () Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-contents () Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-contents-no-properties () Todo))

;;; ============================================================
;;; Minibuffer input

(et-declare
;; AUTHORING STUB: not yet classified.
(@def read-from-minibuffer (prompt: Todo &optional initial-contents: Todo keymap: Todo read: Todo hist: Todo default-value: Todo inherit-input-method: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-string (prompt: Todo &optional initial-input: Todo history: Todo default-value: Todo inherit-input-method: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-command (prompt: Todo &optional default-value: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-variable (prompt: Todo &optional default-value: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-buffer (prompt: Todo &optional def: Todo require-match: Todo predicate: Todo) Todo))

;;; ============================================================
;;; Completion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def try-completion (string: Todo collection: Todo &optional predicate: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def all-completions (string: Todo collection: Todo &optional predicate: Todo hide-spaces: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def completing-read (prompt: Todo collection: Todo &optional predicate: Todo require-match: Todo initial-input: Todo hist: Todo def: Todo inherit-input-method: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def test-completion (string: Todo collection: Todo &optional predicate: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-complete-buffer (string: Todo predicate: Todo flag: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def assoc-string (key: Todo list: Todo &optional case-fold: Todo) Todo))

;;; ============================================================
;;; Minibuffer information

(et-declare
;; AUTHORING STUB: not yet classified.
(@def minibuffer-depth () Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-prompt () Todo))

;;; ============================================================
