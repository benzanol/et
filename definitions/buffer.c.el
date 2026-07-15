;; Type definitions for builtins defined in Emacs' src/buffer.c.

;;; Buffer variables

;; These are ordinary public buffer-local variables.  Variables whose
;; values are internal undo data or file-locking state are intentionally
;; left out until there are useful structural types for them.

(et-declare
 (@variable buffer-file-name String|Nil)
 (@variable default-directory String)
 (@variable enable-multibyte-characters Boolean)
 (@variable buffer-read-only Boolean)
 (@variable buffer-backed-up Boolean)
 (@variable buffer-saved-size Integer)
 (@variable buffer-auto-save-file-name String|Nil)
 (@variable buffer-file-format ListR<Symbol>|Nil)
 (@variable buffer-file-coding-system Symbol)
 (@variable buffer-display-time Any)
 (@variable buffer-display-count Integer))


;;; Buffer lookup and creation

(et-declare
 (@alias BufferOrName (or Buffer String))

 (@function current-buffer () (@return Buffer))
 (@function get-buffer (buffer-or-name)
            (buffer-or-name BufferOrName)
            (@return Buffer|Nil))
 (@function get-file-buffer (filename)
            (filename String)
            (@return Buffer|Nil))
 (@function get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
            (buffer-or-name BufferOrName) (inhibit-buffer-hooks Any)
            (@return Buffer))
 (@function generate-new-buffer (name &optional inhibit-buffer-hooks)
            (name String) (inhibit-buffer-hooks Any)
            (@return Buffer)))

(et-test
 (et-assert-resolve Buffer (current-buffer))
 (et-assert-call Buffer|Nil get-buffer BufferOrName)
 (et-assert-call Buffer get-buffer-create String)
 (et-assert-call Buffer generate-new-buffer String)
 (et-assert-call-errors get-buffer Integer))


;;; Buffer properties

(et-declare
 (@function buffer-name (&optional buffer)
            (buffer Buffer|Nil)
            (@return String|Nil))
 (@function buffer-file-name (&optional buffer)
            (buffer Buffer|Nil)
            (@return String|Nil))
 (@function buffer-base-buffer (&optional buffer)
            (buffer Buffer|Nil)
            (@return Buffer|Nil))
 (@function buffer-list (&optional frame)
            (frame Frame|Nil)
            (@return ListFresh<Buffer>))
 (@function other-buffer (&optional buffer visible-ok frame)
            (buffer Buffer|Nil) (visible-ok Any) (frame Frame|Nil)
            (@return Buffer|Nil))
 (@function buffer-live-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Buffer)))
                         (and Nil (bindsof (subtract T Buffer))))))
 (@function buffer-modified-p (&optional buffer)
            (buffer Buffer|Nil)
            (@return Boolean)))

(et-test
 (et-assert-resolve String|Nil (buffer-name))
 (et-assert-resolve String|Nil (buffer-file-name))
 (et-assert-resolve Buffer|Nil (buffer-base-buffer))
 (et-assert-resolve ListFresh<Buffer> (buffer-list))
 (et-assert-resolve Boolean (buffer-live-p (current-buffer)))
 (et-assert-call Boolean buffer-modified-p Buffer))


;;; Current buffer and lifecycle

(et-declare
 (@function set-buffer (buffer-or-name)
            (buffer-or-name BufferOrName)
            (@return Buffer))
 (@function kill-buffer (&optional buffer-or-name)
            (buffer-or-name BufferOrName|Nil)
            (@return Boolean))
 (@function bury-buffer (&optional buffer-or-name)
            (buffer-or-name BufferOrName|Nil)
            (@return Nil))
 (@function unbury-buffer () (@return Nil)))

(et-test
 (et-assert-call Buffer set-buffer BufferOrName)
 (et-assert-call Boolean kill-buffer)
 (et-assert-call Nil bury-buffer)
 (et-assert-call Nil unbury-buffer))


;;; Buffer-local variables

(et-declare
 (@function buffer-local-value (variable buffer)
            (variable Symbol) (buffer Buffer)
            (@return Any))
 (@function buffer-local-boundp (variable buffer)
            (variable Symbol) (buffer Buffer)
            (@return Boolean))
 (@function buffer-local-variables (&optional buffer)
            (buffer Buffer|Nil)
            (@return ListFresh<Any>)))

(et-test
 (et-assert-call Any buffer-local-value Symbol Buffer)
 (et-assert-call Boolean buffer-local-boundp Symbol Buffer)
 (et-assert-call ListFresh<Any> buffer-local-variables Buffer))


;;; Modification and undo toggles

(et-declare
 (@function set-buffer-modified-p (flag)
            (flag Any)
            (@return Nil))
 (@function restore-buffer-modified-p (flag)
            (flag Any)
            (@return Nil))
 (@function buffer-enable-undo (&optional buffer)
            (buffer Buffer|Nil)
            (@return Nil))
 (@function buffer-disable-undo (&optional buffer)
            (buffer Buffer|Nil)
            (@return Nil)))

(et-test
 (et-assert-resolve Nil (set-buffer-modified-p nil))
 (et-assert-resolve Nil (restore-buffer-modified-p 'autosaved))
 (et-assert-call Nil buffer-enable-undo Buffer)
 (et-assert-call Nil buffer-disable-undo Buffer))
