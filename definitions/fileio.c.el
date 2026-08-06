;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; File name handling

(et-declare
 (@def find-file-name-handler
       (filename: String operation: Symbol)
       AnyFn|Nil)

 (@def file-name-directory (filename: String) String|Nil)

 (@def file-name-nondirectory (filename: String) String)

 (@def unhandled-file-name-directory (filename: String) String|Nil)

 (@def file-name-as-directory (file: String) String)

 (@def directory-name-p (name: String) Boolean)

 (@def directory-file-name (directory: String) String)

 (@def make-temp-file-internal
       (prefix: String dir-flag: Any suffix: String text: String|Nil)
       String)

 (@def make-temp-name (prefix: String) String)

 (@def file-name-concat
       (directory: String|Nil &rest components: &List<String|Nil>)
       String)

 (@def expand-file-name
       (name: String &optional default-directory: String|Nil)
       String)

 (@def substitute-in-file-name (filename: String) String))

;;; ============================================================
;;; File operations

(et-declare
 (@def copy-file
       (file: String newname: String
             &optional ok-if-already-exists: Any keep-time: Any
             preserve-uid-gid: Any preserve-permissions: Any)
       Nil)

 (@def make-directory-internal (directory: String) Nil)

 (@def delete-directory-internal (directory: String) Nil)

 (@def delete-file-internal (filename: String) Nil)

 (@def file-name-case-insensitive-p (filename: String) Boolean)

 (@def rename-file
       (file: String newname: String &optional ok-if-already-exists: Any)
       Nil)

 (@def add-name-to-file
       (file: String newname: String &optional ok-if-already-exists: Any)
       Nil)

 (@def make-symbolic-link
       (target: String linkname: String &optional ok-if-already-exists: Any)
       Nil))

;;; ============================================================
;;; File status and access

(et-declare
 (@def file-name-absolute-p (filename: String) Boolean)

 (@def file-exists-p (filename: String) Boolean)

 (@def file-executable-p (filename: String) Boolean)

 (@def file-readable-p (filename: String) Boolean)

 (@def file-writable-p (filename: String) Boolean)

 (@def access-file (filename: String string: String) Nil)

 (@def file-symlink-p (filename: String) String|Nil)

 (@def file-directory-p (filename: String) Boolean)

 (@def file-accessible-directory-p (filename: String) Boolean)

 (@def file-regular-p (filename: String) Boolean))

;;; ============================================================
;;; File metadata

(et-declare
 (@def file-selinux-context
       (filename: String)
       (Tuple String|Nil String|Nil String|Nil String|Nil))

 (@def set-file-selinux-context
       (filename: String
        context: (&Tuple String|Nil String|Nil String|Nil String|Nil))
       Boolean)

 (@def file-acl (filename: String) String|Nil)

 (@def set-file-acl (filename: String acl-string: String) Boolean)

 (@def file-modes
       (filename: String &optional flag: Nil|@nofollow)
       Integer|Nil)

 (@def set-file-modes
       (filename: String mode: Integer &optional flag: Nil|@nofollow)
       Nil)

 (@def set-default-file-modes (mode: Integer) Nil)

 (@def default-file-modes () Integer)

 (@def set-file-times
       (filename: String
        &optional timestamp: (or Nil Number
                                 (&Cons Integer Integer)
                                 (&Cons Integer (&Cons Integer Integer))
                                 (&Tuple Integer Integer)
                                 (&Tuple Integer Integer Integer)
                                 (&Tuple Integer Integer Integer Integer))
        flag: Nil|@nofollow)
       Boolean)

 (@def unix-sync () Nil)

 (@def file-newer-than-file-p (file1: String file2: String) Boolean))

;;; ============================================================
;;; File contents

(et-declare
 (@def insert-file-contents
       (filename: String
                 &optional visit: Any beg: Integer|Nil end: Integer|Nil
                 replace: Any)
       (Tuple String Integer))

 ;; END is a buffer position only when START is a position; it is ignored
 ;; when START is nil or a string. The type language cannot yet express this
 ;; dependency between parameter types.
 (@def write-region
       (start: Nil|String|IntOrMarker end: Todo filename: String
              &optional append: Any visit: Any lockname: String|Nil
              mustbenew: Any)
       Nil)

 (@def car-less-than-car
       (a: &Cons<NumOrMarker~Any> b: &Cons<NumOrMarker~Any>)
       Boolean))

;;; ============================================================
;;; Visited files

(et-declare
 (@def verify-visited-file-modtime (&optional buf: Buffer) Boolean)

 (@def visited-file-modtime
       ()
       (or -1 0 (Cons Integer Integer)
           (Tuple Integer Integer Integer Integer)))

 ;; TIME-FLAG accepts the special fixnum flags -1 and 0, or a Lisp timestamp,
 ;; but other fixnums are rejected even though integers can otherwise be
 ;; timestamps. The type language cannot distinguish this representation- and
 ;; value-dependent subset without integer refinements.
 (@def set-visited-file-modtime (&optional time-flag: Todo) Nil))

;;; ============================================================
;;; Auto-save

(et-declare
 (@def do-auto-save (&optional no-message: Any current-only: Any) Nil)

 (@def set-buffer-auto-saved () Nil)

 (@def clear-buffer-auto-save-failure () Nil)

 (@def recent-auto-save-p () Boolean))

;;; ============================================================
;;; Miscellaneous i/o

(et-declare
 (@def next-read-file-uses-dialog-p () Boolean)

 (@def set-binary-mode
       (stream: (or @stdin @stdout @stderr) mode: Any)
       Boolean)

 (@def file-system-info
       (filename: String)
       Nil|(Tuple Integer Integer Integer)))

;;; ============================================================
