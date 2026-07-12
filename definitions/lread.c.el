;; Type definitions for builtins defined in Emacs' src/lread.c.

;;; Obarrays

;; An obarray is its own emacs datatype. Wherever one is optional, nil
;; stands for the value of the `obarray' variable.

(et-declare
 (@function obarray-make (&optional size) (size Integer) (@return Obarray))
 (@function obarray-clear (obarray) (obarray Obarray) (@return Nil))
 (@function obarrayp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Obarray)))
                         (and Nil (bindsof (subtract T Obarray))))))
 (@function mapatoms (function &optional obarray)
            (function Function<Args<Symbol>~Any>) (obarray Obarray)
            (@return Nil)))

(et-test
 (et-assert-resolve Obarray (obarray-make))
 (et-assert-resolve Nil (obarray-clear (obarray-make)))
 (et-assert-resolve Boolean (obarrayp (obarray-make)))
 (et-assert-resolve Nil (mapatoms #'ignore))
 (et-assert-resolve-errors (obarray-clear nil)))


;;; Interning

(et-declare
 (@function intern (name &optional obarray)
            (name String) (obarray Obarray) (@return Symbol))
 (@function intern-soft (name &optional obarray)
            (name String|Symbol) (obarray Obarray) (@return Symbol|Nil))
 (@function unintern (name obarray)
            (name String|Symbol) (obarray Obarray|Nil) (@return Boolean))
 (@function read-from-string (string &optional start end)
            (string String) (start Integer) (end Integer)
            (@return Cons<Any~Integer>)))

(et-test
 (et-assert-resolve Symbol (intern "foo"))
 (et-assert-resolve Symbol (intern "foo" (obarray-make)))
 (et-assert-resolve Symbol|Nil (intern-soft "foo"))
 (et-assert-resolve-errors (intern 5))
 (et-assert-resolve-errors (intern "foo" 5))
 (et-assert-resolve Boolean (unintern "foo" nil))
 (et-assert-resolve Cons<Any~Integer> (read-from-string "foo")))
