;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Jit lock mode

(et-declare
 (@def jit-lock-mode (arg: Bool) Any)
 (@def jit-lock-debug-mode (&optional arg: Integer|@toggle?) Boolean)
 (@def jit-lock-register (fun: fn2<Integer~Integer> &optional contextual: Bool) Any)
 (@def jit-lock-unregister (fun: fn2<Integer~Integer>) Any)
 (@def jit-lock-refontify (&optional beg: IntOrMarker? end: IntOrMarker?) Any))

;;; ============================================================
;;; On demand fontification

(et-declare
 (@def jit-lock-function (start: IntOrMarker) Any)
 (@def jit-lock-fontify-now (&optional start: IntOrMarker? end: IntOrMarker?) Any)
 (@def jit-lock-force-redisplay (start: Marker end: IntOrMarker) Any))

;;; ============================================================
;;; Stealth fontification

(et-declare
 (@def jit-lock-stealth-chunk-start (around: IntOrMarker) Integer?)
 (@def jit-lock-stealth-fontify (&optional repeat: Bool) Any))

;;; ============================================================
;;; Deferred fontification

(et-declare
 (@def jit-lock-deferred-fontify () Any)
 (@def jit-lock-context-fontify () Any)
 (@def jit-lock-after-change (start: Integer end: Integer old-len: Integer) Any))

;;; ============================================================
