;; Type definitions for builtins defined in Emacs' src/editfns.c.
;; (Only those whose argument/return types use already-modelled
;; datatypes; buffer/marker functions are out of scope.)

;;; Formatting and messages

(et-declare
 (@function format (string &rest objects)
            (string String) (objects ListR<Any>) (@return String))
 (@function format-message (string &rest objects)
            (string String) (objects ListR<Any>) (@return String))
 (@function message (string &rest objects)
            (string String|Nil) (objects ListR<Any>) (@return String|Nil))
 (@function message-box (string &rest objects)
            (string String|Nil) (objects ListR<Any>) (@return String|Nil))
 (@function message-or-box (string &rest objects)
            (string String|Nil) (objects ListR<Any>) (@return String|Nil))
 (@function current-message () (@return String|Nil))
 (@function propertize (string &rest properties)
            (string String) (properties KVPList<Symbol~Any>) (@return String))
 (@function ngettext (msgid msgid-plural n)
            (msgid String) (msgid-plural String) (n Integer) (@return String)))

(et-test
 (et-assert-resolve String (format "%d apples" 5))
 (et-assert-resolve String (format-message "%s" 'x))
 (et-assert-resolve String|Nil (message "hi"))
 (et-assert-resolve-errors (format 5)))

;;; Characters and strings

(et-declare
 (@function char-to-string (char) (char Integer) (@return String))
 (@function byte-to-string (byte) (byte Integer) (@return String))
 (@function string-to-char (string) (string String) (@return Integer))
 (@function char-equal (c1 c2) (c1 Integer) (c2 Integer) (@return Boolean)))

(et-test
 (et-assert-resolve String (char-to-string ?a))
 (et-assert-resolve Integer (string-to-char "abc"))
 (et-assert-resolve Boolean (char-equal ?a ?A)))

;;; System / user info

(et-declare
 (@function group-name (gid) (gid Number) (@return String|Nil)))
