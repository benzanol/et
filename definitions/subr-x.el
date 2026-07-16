;; Type definitions for lisp/emacs-lisp/subr-x.el.

;;; Display width

;; The pixel width of a string as it would be displayed, optionally using
;; the face remappings and other display settings of BUFFER.

(et-declare
 (@function string-pixel-width (string &optional buffer)
            (string String) (buffer Buffer|Nil) (@return Integer)))

(et-test
 (et-assert-resolve Integer (string-pixel-width "hello"))
 (et-assert-resolve Integer
  (string-pixel-width (:type String) (:type Buffer)))
 (et-assert-resolve-errors (string-pixel-width ?a)))
