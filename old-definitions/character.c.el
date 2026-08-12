;; Type definitions for builtins defined in Emacs' src/character.c.
;;
;; Characters are character codes, i.e. plain Integers -- there is no
;; distinct character datatype, so `characterp' narrows to `Integer' and
;; every character-valued result is an `Integer'.


;;; Predicates

(et-declare
 (@function characterp (object &optional ignore)
            (@generics [T]) (object T) (ignore Any)
            (@return (or (and True (bindsof (and T Integer)))
                         (and Nil (bindsof (subtract T Integer)))))))

(et-test
 (et-assert-resolve Boolean (characterp ?a))
 (et-subtype? (et-root-check-type '(let* ((c (et: String|Integer 4)))
                  (if (characterp c) c 0)))
              (et Integer)))


;;; Character codes

(et-declare
 (@function max-char (&optional unicode) (unicode Any) (@return Integer))
 (@function unibyte-char-to-multibyte (ch) (ch Integer) (@return Integer))
 (@function multibyte-char-to-unibyte (ch) (ch Integer) (@return Integer))
 (@function char-resolve-modifiers (char) (char Integer) (@return Integer))
 (@function get-byte (&optional position string)
            (position Integer|Nil) (string String|Nil) (@return Integer)))

(et-test
 (et-assert-resolve Integer (max-char))
 (et-assert-resolve Integer (char-resolve-modifiers ?a))
 (et-assert-resolve Integer (get-byte 1 "a"))
 (et-assert-resolve-errors (unibyte-char-to-multibyte "a")))


;;; Display width

(et-declare
 (@function char-width (char) (char Integer) (@return Integer))
 (@function string-width (string &optional from to)
            (string String) (from Integer|Nil) (to Integer|Nil) (@return Integer)))

(et-test
 (et-assert-resolve Integer (char-width ?a))
 (et-assert-resolve Integer (string-width "hello" 1 3))
 (et-assert-resolve-errors (string-width ?a)))


;;; Constructors

(et-declare
 (@function string (&rest characters)
            (characters ListR<Integer>) (@return String))
 (@function unibyte-string (&rest bytes)
            (bytes ListR<Integer>) (@return String)))

(et-test
 (et-assert-resolve String (string ?a ?b))
 (et-assert-resolve String (unibyte-string))
 (et-assert-resolve-errors (string "a")))
