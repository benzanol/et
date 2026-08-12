;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Mutexes

(et-declare
;; AUTHORING STUB: not yet classified.
(@def make-mutex (&optional name: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def mutex-lock (mutex: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def mutex-unlock (mutex: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def mutex-name (mutex: Todo) Todo))

;;; ============================================================
;;; Condition variables

(et-declare
;; AUTHORING STUB: not yet classified.
(@def make-condition-variable (mutex: Todo &optional name: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def condition-wait (cond: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def condition-notify (cond: Todo &optional all: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def condition-mutex (cond: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def condition-name (cond: Todo) Todo))

;;; ============================================================
;;; Threads

(et-declare
;; AUTHORING STUB: not yet classified.
(@def thread-yield () Todo)
;; AUTHORING STUB: not yet classified.
(@def make-thread (function: Todo &optional name: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def current-thread () Todo)
;; AUTHORING STUB: not yet classified.
(@def thread-name (thread: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def thread-signal (thread: Todo error-symbol: Todo data: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def thread-live-p (thread: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def thread-join (thread: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def all-threads () Todo)
;; AUTHORING STUB: not yet classified.
(@def thread-last-error (&optional cleanup: Todo) Todo))

;;; ============================================================
