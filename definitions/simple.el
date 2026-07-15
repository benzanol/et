;; Type definitions for public APIs defined in lisp/simple.el.

;;; Editing variables

(et-declare
 (@variable inhibit-field-text-motion Boolean)
 (@variable line-move-visual Boolean)
 (@variable line-number-display-limit Integer|Nil)
 (@variable line-number-display-limit-width Integer)
 (@variable next-line-add-newlines Boolean)
 (@variable kill-whole-line Boolean)
 (@variable yank-excluded-properties ListR<Symbol>)
 (@variable deactivate-mark Boolean)
 (@variable mark-active Boolean)
 (@variable transient-mark-mode Boolean)
 (@variable set-mark-command-repeat-pop Boolean)
 (@variable shell-command-switch String)
 (@variable async-shell-command-buffer Symbol|String)
 (@variable major-mode Symbol)
 (@variable mode-name String|Symbol)
 (@variable indent-tabs-mode Boolean)
 (@variable tab-width Integer)
 (@variable fill-column Integer))


;;; Region and word helpers

(et-declare
 (@function mark-whole-buffer () (@return Nil))
 (@function count-words (start end &optional totals)
            (start IntOrMarker) (end IntOrMarker) (totals Any)
            (@return Integer))
 (@function count-words-region (start end &optional arg)
            (start IntOrMarker) (end IntOrMarker) (arg Any)
            (@return Integer))
 (@function mark (&optional force)
            (force Any)
            (@return Integer|Nil))
 (@function what-line () (@return Nil))
 (@function line-number-at-pos (&optional position absolute)
            (position IntOrMarker|Nil) (absolute Any)
            (@return Integer))
 (@function current-word (&optional strict really-word)
            (strict Any) (really-word Any)
            (@return String|Nil))
 (@function region-active-p () (@return Boolean))
 (@function use-region-p () (@return Boolean)))

(et-test
 (et-assert-resolve Nil (mark-whole-buffer))
 (et-assert-call Integer count-words IntOrMarker IntOrMarker)
 (et-assert-call Integer count-words-region IntOrMarker IntOrMarker)
 (et-assert-resolve Integer|Nil (mark))
 (et-assert-resolve Integer (line-number-at-pos))
 (et-assert-resolve String|Nil (current-word))
 (et-assert-resolve Boolean (region-active-p))
 (et-assert-resolve Boolean (use-region-p)))


;;; Line movement and insertion

(et-declare
 (@function delete-indentation (&optional arg beg end)
            (arg Any) (beg IntOrMarker|Nil) (end IntOrMarker|Nil)
            (@return Nil))
 (@function newline (&optional arg interactive)
            (arg Integer|Nil) (interactive Any)
            (@return Nil))
 (@function open-line (n)
            (n Integer)
            (@return Nil))
 (@function split-line (&optional arg)
            (arg Integer|Nil)
            (@return Nil))
 (@function quoted-insert (arg)
            (arg Integer|Nil)
            (@return Nil))
 (@function forward-to-indentation (&optional arg)
            (arg Integer|Nil)
            (@return Nil))
 (@function back-to-indentation () (@return Nil))
 (@function beginning-of-line (&optional n)
            (n Integer|Nil)
            (@return Nil))
 (@function end-of-line (&optional n)
            (n Integer|Nil)
            (@return Nil))
 (@function forward-line (&optional n)
            (n Integer|Nil)
            (@return Integer))
 (@function next-line (&optional arg try-vscroll)
            (arg Integer|Nil) (try-vscroll Any)
            (@return Nil))
 (@function previous-line (&optional arg try-vscroll)
            (arg Integer|Nil) (try-vscroll Any)
            (@return Nil)))

(et-test
 (et-assert-resolve Nil (delete-indentation))
 (et-assert-resolve Nil (newline))
 (et-assert-call Nil open-line Integer)
 (et-assert-resolve Nil (split-line))
 (et-assert-call Nil quoted-insert Integer)
 (et-assert-resolve Nil (forward-to-indentation))
 (et-assert-resolve Nil (back-to-indentation))
 (et-assert-resolve Nil (beginning-of-line))
 (et-assert-resolve Nil (end-of-line))
 (et-assert-resolve Integer (forward-line))
 (et-assert-resolve Nil (next-line))
 (et-assert-resolve Nil (previous-line)))


;;; Killing and copying text

(et-declare
 (@function kill-line (&optional arg)
            (arg Integer|Nil)
            (@return Nil))
 (@function kill-whole-line (&optional arg)
            (arg Integer|Nil)
            (@return Nil))
 (@function current-kill (n &optional do-not-move)
            (n Integer) (do-not-move Any)
            (@return String))
 (@function kill-new (string &optional replace)
            (string String) (replace Any)
            (@return Nil))
 (@function append-to-buffer (buffer start end)
            (buffer BufferOrName) (start IntOrMarker) (end IntOrMarker)
            (@return Nil))
 (@function prepend-to-buffer (buffer start end)
            (buffer BufferOrName) (start IntOrMarker) (end IntOrMarker)
            (@return Nil))
 (@function copy-to-buffer (buffer start end)
            (buffer BufferOrName) (start IntOrMarker) (end IntOrMarker)
            (@return Nil)))

(et-test
 (et-assert-resolve Nil (kill-line))
 (et-assert-resolve Nil (kill-whole-line))
 (et-assert-call String current-kill Integer)
 (et-assert-call Nil kill-new String)
 (et-assert-call Nil append-to-buffer BufferOrName IntOrMarker IntOrMarker)
 (et-assert-call Nil prepend-to-buffer BufferOrName IntOrMarker IntOrMarker)
 (et-assert-call Nil copy-to-buffer BufferOrName IntOrMarker IntOrMarker))


;;; Shell helpers and modes

(et-declare
 (@function shell-command-to-string (command)
            (command String)
            (@return String))
 (@function undo-auto-amalgamate () (@return Nil))
 (@function fundamental-mode () (@return Nil)))

(et-test
 (et-assert-call String shell-command-to-string String)
 (et-assert-resolve Nil (undo-auto-amalgamate))
 (et-assert-resolve Nil (fundamental-mode)))
