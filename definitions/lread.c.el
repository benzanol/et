;;; lread.c.el --- Type definitions for src/lread.c -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools


;;; Commentary:

;; Type definitions for builtins defined in Emacs' src/lread.c.


;;; Code:

(require 'et-check)


(et-declare
 (@function intern (name &optional obarray)
            (name String) (obarray Any) (@return Symbol))
 (@function intern-soft (name &optional obarray)
            (name String|Symbol) (obarray Any) (@return Symbol|Nil)))

(et-test
 (et-assert-resolve Symbol (intern "foo"))
 (et-assert-resolve Symbol|Nil (intern-soft "foo"))
 (et-assert-resolve-errors (intern 5)))


(provide 'lread.c)
;;; lread.c.el ends here
