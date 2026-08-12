;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Keyboard macro recording

(et-declare
 (@def start-kbd-macro (append: Any &optional no-exec: Bool) Nil)
 (@def end-kbd-macro (&optional repeat: Integer? loopfunc: fn<Nil~Any>?) Nil)
 (@def cancel-kbd-macro-events () Nil)
 (@def store-kbd-macro-event (event: Any) Nil))

;;; ============================================================
;;; Keyboard macro execution

(et-declare
 (@def call-last-kbd-macro (&optional prefix: Any loopfunc: fn<Nil~Any>?) Nil)
 (@def execute-kbd-macro
       (macro: Symbol|String|Vector &optional count: Integer? loopfunc: fn<Nil~Any>?)
       Nil))

;;; ============================================================
