;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Character output

(et-declare
 (@def write-char
       (character: Integer &optional printcharfun: True|Buffer|Marker|fn<Integer~Any>?)
       Integer))

;;; ============================================================
;;; Object printing

(et-declare
 (@def terpri
       (&optional printcharfun: True|Buffer|Marker|fn<Integer~Any>? ensure: Bool)
       Boolean)
 (@def prin1
       ([T] object: T
        &optional printcharfun: True|Buffer|Marker|fn<Integer~Any>?
        overrides: True|List<Cons<Symbol~Any>>?)
       T)
 (@def prin1-to-string
       (object: Any &optional noescape: Bool overrides: True|List<Cons<Symbol~Any>>?)
       String)
 (@def princ
       ([T] object: T &optional printcharfun: True|Buffer|Marker|fn<Integer~Any>?)
       T)
 (@def print
       ([T] object: T &optional printcharfun: True|Buffer|Marker|fn<Integer~Any>?)
       T)
 (@def flush-standard-output () Nil)
 (@def external-debugging-output (character: Integer) Integer)
 (@def redirect-debugging-output (file: String? &optional append: Bool) Nil))

;;; ============================================================
;;; Error messages

(et-declare
 (@def error-message-string (obj: Cons<Symbol~Any>) String))

;;; ============================================================
;;; Print preprocessing

(et-declare
 (@def print--preprocess (object: Any) Nil))

;;; ============================================================
