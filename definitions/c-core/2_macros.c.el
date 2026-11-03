;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Keyboard macro recording

(et-declare
 (@def start-kbd-macro (append: Bool &optional no-exec: Bool) Nil)
 (@def end-kbd-macro (&optional repeat: Integer? loopfunc: AnyFn?) Nil)
 (@def cancel-kbd-macro-events () Nil)
 (@def store-kbd-macro-event (event: Any) Nil))

;;; ============================================================
;;; Keyboard macro execution

(et-declare
 (@def call-last-kbd-macro (&optional prefix: Nil|@-|Cons<Integer_Any>|Integer loopfunc: AnyFn?) Nil)
 (@def execute-kbd-macro
       (macro: Symbol|String|Vector &optional count: Nil|@-|Cons<Integer_Any>|Integer loopfunc: AnyFn?)
       Nil))

;;; ============================================================
