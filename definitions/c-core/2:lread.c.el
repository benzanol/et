;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; File loading

(et-declare
 (@def get-load-suffixes () ListFresh<String>)
 (@def load
       (file: String &optional noerror: Bool nomessage: Bool nosuffix: Bool must-suffix: Bool)
       Boolean)
 (@def locate-file-internal
       (filename: String path: &List<String> &optional suffixes: &List<String>? predicate: AnyFn|Integer?)
       String?))

;;; ============================================================
;;; Evaluation and reading

(et-declare
 (@def eval-buffer
       (&optional buffer: Buffer|String? printflag: Bool filename: String? unibyte: Any do-allow-print: Bool)
       Nil)
 (@def eval-region
       (start: IntOrMarker end: IntOrMarker &optional printflag: Bool read-function: (or Nil (fn (Args Any) Any)))
       Nil)
 (@def read (&optional stream: Buffer|Marker|String|AnyFn|True?) Sexp)
 (@def read-positioning-symbols (&optional stream: Buffer|Marker|String|AnyFn|True?) Sexp)
 (@def read-from-string (string: String &optional start: Integer? end: Integer?) ConsFresh<Sexp~Integer>))

;;; ============================================================
;;; Symbols and obarrays

(et-declare
 (@def intern (string: String &optional obarray: Obarray?) Symbol)
 (@def intern-soft (name: String|Symbol &optional obarray: Obarray?) Symbol?)
 (@def unintern (name: String|Symbol obarray: Obarray?) Boolean)
 (@def obarray-make (&optional size: Integer?) Obarray)
 (@def obarrayp (object: Any) Boolean)
 (@def obarray-clear (obarray: Obarray) Nil)
 (@def mapatoms (function: (fn (Args Symbol) Any) &optional obarray: Obarray?) Nil))

;;; ============================================================
