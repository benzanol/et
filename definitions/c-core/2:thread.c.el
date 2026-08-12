;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Mutexes

(et-declare
 (@def make-mutex (&optional name: String?) Mutex)
 (@def mutex-lock (mutex: Mutex) Nil)
 (@def mutex-unlock (mutex: Mutex) Nil)
 (@def mutex-name (mutex: Mutex) String?))

;;; ============================================================
;;; Condition variables

(et-declare
 (@def make-condition-variable (mutex: Mutex &optional name: String?) ConditionVariable)
 (@def condition-wait (cond: ConditionVariable) Nil)
 (@def condition-notify (cond: ConditionVariable &optional all: Bool) Nil)
 (@def condition-mutex (cond: ConditionVariable) Mutex)
 (@def condition-name (cond: ConditionVariable) String?))

;;; ============================================================
;;; Threads

(et-declare
 (@def thread-yield () Nil)
 (@def make-thread (function: fn<Nil~Any> &optional name: String? buffer-disposition: Boolean|@silently) Thread)
 (@def current-thread () Thread)
 (@def thread-name (thread: Thread) String?)
 (@def thread-signal (thread: Thread error-symbol: NonNilSymbol data: Any) Nil)
 (@def thread-live-p (thread: Thread) Boolean)
 (@def thread-buffer-disposition (thread: Thread) Boolean|@silently)
 (@def thread-set-buffer-disposition ([(<= V Boolean|@silently)] thread: Thread value: V) V)
 (@def thread-join (thread: Thread) Any)
 (@def all-threads () ListFresh<Thread>)
 (@def thread-last-error (&optional cleanup: Bool) Any))

;;; ============================================================
