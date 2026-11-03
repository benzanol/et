;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Column and indentation

(et-declare
 (@def current-column () Integer)
 (@def indent-to (column: Integer &optional minimum: Integer?) Integer)
 (@def current-indentation () Integer)
 (@def move-to-column (column: Integer &optional force: Bool) Integer))

;;; ============================================================
;;; Screen motion

(et-declare
 (@def compute-motion
       (from: IntOrMarker frompos: &Cons<Integer_Integer> to: IntOrMarker
        topos: &Cons<Integer_Integer>? width: Integer? offsets: &Cons<Integer_Integer>?
        window: Window?)
       (tuple Integer Integer Integer Integer Boolean))
 (@def line-number-display-width (&optional pixelwise: Bool) Integer|Float)
 (@def vertical-motion
       (lines: Integer|Cons<Number_Integer> &optional window: Window? cur-col: Number?)
       Integer))

;;; ============================================================
