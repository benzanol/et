;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Documentation access

(et-declare
 (@def documentation-stringp (object: Any) Boolean)
 (@def documentation (function: Symbol|AnyFn &optional raw: Bool) String?)
 (@def internal-subr-documentation (function: AnyFn) Integer|String|Boolean)
 (@def documentation-property (symbol: Symbol prop: Symbol &optional raw: Bool) Any))

;;; ============================================================
;;; Documentation file scanning

(et-declare
 (@def Snarf-documentation (filename: String) Nil))

;;; ============================================================
;;; Text quoting style

(et-declare
 (@def text-quoting-style () @grave|@straight|@curve))

;;; ============================================================
