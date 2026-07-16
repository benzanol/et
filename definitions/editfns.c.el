;; Type definitions for builtins defined in Emacs' src/editfns.c.
;;
;; Anywhere editfns takes a buffer position, it accepts an `IntOrMarker':
;; either an integer, or a marker which it converts to its integer
;; position. Positions are only ever *returned* as integers, except by the
;; explicitly marker-valued functions (`point-marker' and friends).

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

;;; Point and positions

(et-declare
 (@function point () (@return Integer))
 ;; `goto-char' hands back the position it was given, unchanged -- so a
 ;; marker in, a marker out.
 (@function goto-char (position)
            (@generics [(<= P IntOrMarker)]) (position P) (@return P))
 (@function point-min () (@return Integer))
 (@function point-max () (@return Integer))
 (@function region-beginning () (@return Integer))
 (@function region-end () (@return Integer))
 (@function gap-position () (@return Integer))
 (@function gap-size () (@return Integer))
 ;; BUFFER defaults to the current buffer.
 (@function buffer-size (&optional buffer) (buffer Buffer|Nil) (@return Integer))

 ;; Nil when the position is outside the accessible portion.
 (@function position-bytes (position) (position IntOrMarker) (@return Integer|Nil))
 (@function byte-to-position (bytepos) (bytepos Integer) (@return Integer|Nil))

 ;; N is a line count, not a position.
 (@function pos-bol (&optional n) (n Integer|Nil) (@return Integer))
 (@function pos-eol (&optional n) (n Integer|Nil) (@return Integer))
 (@function line-beginning-position (&optional n) (n Integer|Nil) (@return Integer))
 (@function line-end-position (&optional n) (n Integer|Nil) (@return Integer)))

(et-test
 (et-assert-resolve Integer (point))
 (et-assert-resolve Integer (goto-char (point-min)))
 (et-assert-resolve Integer (pos-bol 2))
 (et-assert-resolve Integer|Nil (position-bytes 1))
 (et-assert-resolve Integer (buffer-size))
 (et-assert-resolve Marker
  (goto-char (:type Marker)))
 (et-assert-resolve-errors (goto-char "1")))


;;; Markers into the current buffer

(et-declare
 (@function point-marker () (@return Marker))
 (@function point-min-marker () (@return Marker))
 (@function point-max-marker () (@return Marker))
 ;; The buffer's mark. Moving this marker moves the mark itself.
 (@function mark-marker () (@return Marker)))

(et-test
 (et-assert-resolve Marker (point-marker))
 (et-assert-resolve Marker (goto-char (point-max-marker))))


;;; Characters at point

(et-declare
 ;; `following-char'/`preceding-char' return 0 at the buffer's edge.
 (@function following-char () (@return Integer))
 (@function preceding-char () (@return Integer))
 ;; `char-after'/`char-before' return nil outside the accessible portion.
 (@function char-after (&optional pos) (pos IntOrMarker|Nil) (@return Integer|Nil))
 (@function char-before (&optional pos) (pos IntOrMarker|Nil) (@return Integer|Nil))

 (@function bobp () (@return Boolean))
 (@function eobp () (@return Boolean))
 (@function bolp () (@return Boolean))
 (@function eolp () (@return Boolean)))

(et-test
 (et-assert-resolve Integer|Nil (char-after))
 (et-assert-resolve Integer (following-char))
 (et-assert-resolve Boolean (bobp)))


;;; Buffer contents

(et-declare
 (@function buffer-substring (start end)
            (start IntOrMarker) (end IntOrMarker) (@return String))
 (@function buffer-substring-no-properties (start end)
            (start IntOrMarker) (end IntOrMarker) (@return String))
 (@function buffer-string () (@return String))
 (@function delete-and-extract-region (start end)
            (start IntOrMarker) (end IntOrMarker) (@return String))
 (@function delete-region (start end)
            (start IntOrMarker) (end IntOrMarker) (@return Nil))
 (@function subst-char-in-region (start end fromchar tochar &optional noundo)
            (start IntOrMarker) (end IntOrMarker) (fromchar Integer) (tochar Integer)
            (noundo Any) (@return Nil))
 (@function transpose-regions (startr1 endr1 startr2 endr2 &optional leave-markers)
            (startr1 IntOrMarker) (endr1 IntOrMarker) (startr2 IntOrMarker) (endr2 IntOrMarker)
            (leave-markers Any) (@return Nil))

 ;; BUFFER may be a buffer or the name of one; START and END default to
 ;; the accessible portion of it.
 (@function insert-buffer-substring (buffer &optional start end)
            (buffer Buffer|String) (start IntOrMarker|Nil) (end IntOrMarker|Nil) (@return Nil))
 ;; Every argument may be nil: a nil buffer means the current buffer, and
 ;; a nil position means that buffer's `point-min'/`point-max'.
 (@function compare-buffer-substrings (buffer1 start1 end1 buffer2 start2 end2)
            (buffer1 Buffer|Nil) (start1 IntOrMarker|Nil) (end1 IntOrMarker|Nil)
            (buffer2 Buffer|Nil) (start2 IntOrMarker|Nil) (end2 IntOrMarker|Nil)
            (@return Integer)))

(et-test
 (et-assert-resolve String (buffer-substring 1 10))
 (et-assert-resolve String (delete-and-extract-region 1 10))
 (et-assert-resolve String (buffer-substring (point-min-marker) (point-max)))
 (et-assert-resolve Nil (delete-region 1 10))
 (et-assert-resolve Nil (insert-buffer-substring "*scratch*"))
 (et-assert-resolve Integer (compare-buffer-substrings nil nil nil nil nil nil))
 (et-assert-resolve-errors (buffer-substring 1))
 (et-assert-resolve-errors (insert-buffer-substring 5)))


;;; Insertion

;; The insertion functions take any mix of strings and characters, and
;; all of them return nil.

(et-declare
 (@function insert (&rest args) (args ListR<String|Integer>) (@return Nil))
 (@function insert-and-inherit (&rest args) (args ListR<String|Integer>) (@return Nil))
 (@function insert-before-markers (&rest args) (args ListR<String|Integer>) (@return Nil))
 (@function insert-before-markers-and-inherit (&rest args)
            (args ListR<String|Integer>) (@return Nil))
 (@function insert-char (character &optional count inherit)
            (character Integer) (count Integer|Nil) (inherit Any) (@return Nil))
 (@function insert-byte (byte &optional count inherit)
            (byte Integer) (count Integer|Nil) (inherit Any) (@return Nil)))

(et-test
 (et-assert-resolve Nil (insert "hi" ?a))
 (et-assert-resolve Nil (insert-char ?a 3))
 (et-assert-resolve-errors (insert 'sym)))


;;; Narrowing

(et-declare
 (@function widen () (@return Nil))
 (@function narrow-to-region (start end)
            (start IntOrMarker) (end IntOrMarker) (@return Nil)))


;;; Fields

(et-declare
 (@function delete-field (&optional pos) (pos IntOrMarker|Nil) (@return Nil))
 (@function field-string (&optional pos) (pos IntOrMarker|Nil) (@return String))
 (@function field-string-no-properties (&optional pos) (pos IntOrMarker|Nil) (@return String))
 (@function field-beginning (&optional pos escape-from-edge limit)
            (pos IntOrMarker|Nil) (escape-from-edge Any) (limit IntOrMarker|Nil) (@return Integer))
 (@function field-end (&optional pos escape-from-edge limit)
            (pos IntOrMarker|Nil) (escape-from-edge Any) (limit IntOrMarker|Nil) (@return Integer)))

(et-test
 (et-assert-resolve String (field-string))
 (et-assert-resolve Integer (field-beginning (point))))


;;; Save macros

;; These evaluate their body like a `progn', restoring some piece of
;; state afterwards, so the value of the last body form is the value of
;; the whole form.

(et-declare
 (@macro save-excursion :progn t)
 (@macro save-current-buffer :progn t)
 (@macro save-restriction :progn t))

(et-test
 (et-assert-resolve String (save-excursion (goto-char 1) "done"))
 (et-assert-resolve Nil (save-restriction)))


;;; System / user info

;; A uid/gid may be a float on systems where it does not fit in a fixnum,
;; hence Number rather than Integer.

(et-declare
 (@function user-login-name (&optional uid) (uid Number|Nil) (@return String|Nil))
 (@function user-real-login-name () (@return String))
 (@function user-full-name (&optional uid) (uid Number|Nil) (@return String|Nil))
 (@function user-uid () (@return Number))
 (@function user-real-uid () (@return Number))
 (@function group-name (gid) (gid Number) (@return String|Nil))
 (@function group-gid () (@return Number))
 (@function group-real-gid () (@return Number))
 (@function system-name () (@return String))
 (@function emacs-pid () (@return Integer)))

(et-test
 (et-assert-resolve String|Nil (user-login-name))
 (et-assert-resolve Number (user-uid))
 (et-assert-resolve Integer (emacs-pid)))
