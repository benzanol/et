;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Recursive editing and errors

(et-declare
;; AUTHORING STUB: not yet classified.
(@def recursive-edit () Todo)
;; AUTHORING STUB: not yet classified.
(@def command-error-default-function (data: Todo context: Todo signal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def top-level () Todo)
;; AUTHORING STUB: not yet classified.
(@def exit-recursive-edit () Todo)
;; AUTHORING STUB: not yet classified.
(@def abort-recursive-edit () Todo))

;;; ============================================================
;;; Idle time

(et-declare
;; AUTHORING STUB: not yet classified.
(@def current-idle-time () Todo))

;;; ============================================================
;;; Event processing

(et-declare
;; AUTHORING STUB: not yet classified.
(@def internal-event-symbol-parse-modifiers (symbol: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def event-convert-list (event-desc: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-handle-focus-in (event: Todo) Todo))

;;; ============================================================
;;; Keyboard input

(et-declare
;; AUTHORING STUB: not yet classified.
(@def read-key-sequence (prompt: Todo &optional continue-echo: Todo dont-downcase-last: Todo can-return-switch-frame: Todo cmd-loop: Todo disable-text-conversion: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def read-key-sequence-vector (prompt: Todo &optional continue-echo: Todo dont-downcase-last: Todo can-return-switch-frame: Todo cmd-loop: Todo disable-text-conversion: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def input-pending-p (&optional check-timers: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def lossage-size (&optional arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def recent-keys (&optional include-cmds: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def this-command-keys () Todo)
;; AUTHORING STUB: not yet classified.
(@def this-command-keys-vector () Todo)
;; AUTHORING STUB: not yet classified.
(@def this-single-command-keys () Todo)
;; AUTHORING STUB: not yet classified.
(@def this-single-command-raw-keys () Todo)
;; AUTHORING STUB: not yet classified.
(@def clear-this-command-keys (&optional keep-record: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def recursion-depth () Todo)
;; AUTHORING STUB: not yet classified.
(@def open-dribble-file (file: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def discard-input () Todo)
;; AUTHORING STUB: not yet classified.
(@def suspend-emacs (&optional stuffstring: Todo) Todo))

;;; ============================================================
;;; Terminal input modes

(et-declare
;; AUTHORING STUB: not yet classified.
(@def set-input-interrupt-mode (interrupt: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-output-flow-control (flow: Todo &optional terminal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-input-meta-mode (meta: Todo &optional terminal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-quit-char (quit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-input-mode (interrupt: Todo flow: Todo meta: Todo &optional quit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def current-input-mode () Todo))

;;; ============================================================
;;; Position queries

(et-declare
;; AUTHORING STUB: not yet classified.
(@def posn-at-x-y (x: Todo y: Todo &optional frame-or-window: Todo whole: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def posn-at-point (&optional pos: Todo window: Todo) Todo))

;;; ============================================================
