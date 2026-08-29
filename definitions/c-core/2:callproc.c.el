;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Synchronous subprocess invocation

(et-declare
 (@def call-process
       (program: String &optional infile: String?
        destination: (or Buffer String Boolean Integer
                         (Tuple @file String)
                         (Tuple Buffer|String|Boolean String|Boolean))
        display: Bool &rest args: &List<String>)
       Nil|Integer|String)
 (@def call-process-region
       (start: IntOrMarker|String? end: IntOrMarker?
        program: String &optional delete: Bool
        buffer: (or Buffer String Boolean Integer
                    (Tuple @file String)
                    (Tuple Buffer|String|Boolean String|Boolean))
        display: Bool &rest args: &List<String>)
       Nil|Integer|String))

;;; ============================================================
;;; Environment lookup

(et-declare
 (@def getenv-internal (variable: String &optional env: &List<String>?) String|Boolean))

;;; ============================================================
