(et-declare
 (@function intern (name &optional obarray)
            (name String) (obarray Any) (@return Symbol))
 (@function intern-soft (name &optional obarray)
            (name String|Symbol) (obarray Any) (@return Symbol|Nil))
 (@function unintern (name obarray)
            (name String|Symbol) (obarray Any) (@return Boolean))
 (@function read-from-string (string &optional start end)
            (string String) (start Integer) (end Integer)
            (@return Cons<Any~Integer>)))

(et-test
 (et-assert-resolve Symbol (intern "foo"))
 (et-assert-resolve Symbol|Nil (intern-soft "foo"))
 (et-assert-resolve-errors (intern 5))
 (et-assert-resolve Boolean (unintern "foo" nil))
 (et-assert-resolve Cons<Any~Integer> (read-from-string "foo")))
