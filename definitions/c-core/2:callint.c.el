;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Interactive invocation

(et-declare
 ;; `interactive' is a declarative special form: its arguments are never
 ;; evaluated, and it only tells `call-interactively' how to read arguments
 ;; for the enclosing command. There is no checker shortcut for a
 ;; declaration-only form whose operands are never evaluated or checked.
 (@check interactive ($todo))
 (@def funcall-interactively ([A R] function: (fn A R) &rest arguments: A) R)
 ;; FUNCTION's arguments are supplied internally from its `interactive'
 ;; spec, not from the caller, so only its return type can be tracked here.
 (@def call-interactively
       ([R] function: Symbol|(fn &List R) &optional record-flag: Bool keys: &Vector<Any>?)
       R))

;;; ============================================================
;;; Prefix arguments

(et-declare
 (@def prefix-numeric-value (raw: Nil|@-|Cons<Integer~Any>|Integer) Integer))

;;; ============================================================
