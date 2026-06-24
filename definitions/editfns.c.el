;;; editfns.c.el --- Type definitions for src/editfns.c -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools


;;; Commentary:

;; Type definitions for builtins defined in Emacs' src/editfns.c.
;; (Only those whose argument/return types use already-modelled
;; datatypes; buffer/marker functions are out of scope.)


;;; Code:

(require 'et-check)


(et-declare
 (@function format (string &rest objects)
            (string String) (objects ListR<Any>) (@return String))
 (@function format-message (string &rest objects)
            (string String) (objects ListR<Any>) (@return String))
 (@function message (string &rest objects)
            (string String|Nil) (objects ListR<Any>) (@return String|Nil))

 (@function char-to-string (char) (char Integer) (@return String))
 (@function string-to-char (string) (string String) (@return Integer)))

(et-test
 (et-assert-resolve String (format "%d apples" 5))
 (et-assert-resolve String (format-message "%s" 'x))
 (et-assert-resolve String|Nil (message "hi"))
 (et-assert-resolve String (char-to-string ?a))
 (et-assert-resolve Integer (string-to-char "abc"))
 (et-assert-resolve-errors (format 5)))


(provide 'editfns.c)
;;; editfns.c.el ends here
