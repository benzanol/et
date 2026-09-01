;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Register value struct

(et-declare
 (@def registerv-p (object: Any) Boolean)
 (@def registerv-data (registerv: *registerv) Any)
 (@def registerv-print-func (registerv: *registerv) fn1<Any>?)
 (@def registerv-jump-func (registerv: *registerv) fn1<Any>?)
 (@def registerv-insert-func (registerv: *registerv) fn1<Any>?)
 (@def registerv-make
       (data: Any &key print-func: fn1<Any>? jump-func: fn1<Any>? insert-func: fn1<Any>?)
       *registerv))

;;; ============================================================
;;; Register storage and configuration

(et-declare
 (@variable register-alist
            (Alist Integer
                   (or Nil String Number Marker *registerv List<String>
                       (Cons @file String)
                       (Tuple @file-query String Integer)
                       (Cons @buffer String)
                       (Tuple WindowConfiguration Marker)
                       (Tuple (Cons @frame-configuration
                                    (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                              Marker))))
 (@variable register-separator Integer?)
 (@variable register-preview-delay Number?)
 (@variable register-preview-default-keys List<String>)
 (@variable register-preview-function
            (or Nil (fn (Args (Cons Integer
                                     (or Nil String Number Marker *registerv List<String>
                                         (Cons @file String)
                                         (Tuple @file-query String Integer)
                                         (Cons @buffer String)
                                         (Tuple WindowConfiguration Marker)
                                         (Tuple (Cons @frame-configuration
                                                      (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                                                Marker))))
                          String)))
 (@variable register-use-preview True|@insist|Nil|@never|@traditional)
 (@def get-register
       (register: Integer)
       (or Nil String Number Marker *registerv List<String>
           (Cons @file String)
           (Tuple @file-query String Integer)
           (Cons @buffer String)
           (Tuple WindowConfiguration Marker)
           (Tuple (Cons @frame-configuration
                        (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                  Marker)))
 (@def set-register ([V] register: Integer value: V) V)
 (@def register-describe-oneline (c: Integer) String))

;;; ============================================================
;;; Register preview

(et-declare
 (@def register-preview-default-1
       (r: (Cons Integer
                 (or Nil String Number Marker *registerv List<String>
                     (Cons @file String)
                     (Tuple @file-query String Integer)
                     (Cons @buffer String)
                     (Tuple WindowConfiguration Marker)
                     (Tuple (Cons @frame-configuration
                                  (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                            Marker))))
       String)
 (@def register-preview-default
       (r: (Cons Integer
                 (or Nil String Number Marker *registerv List<String>
                     (Cons @file String)
                     (Tuple @file-query String Integer)
                     (Cons @buffer String)
                     (Tuple WindowConfiguration Marker)
                     (Tuple (Cons @frame-configuration
                                  (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                            Marker))))
       String)
 (@def make-register-preview-info
       (&key types: &List<@string|@number|@marker|@buffer|@file|@file-query|@window|@frame|@kmacro|@all>?
        msg: String? act: @insert|@jump|@view|@modify|@set? smatch: Boolean noconfirm: Bool)
       *register-preview-info)
 (@def register-preview-info-p (object: Any) Boolean)
 (@def copy-register-preview-info (info: *register-preview-info) *register-preview-info)
 (@def register-preview-info-types
       (info: *register-preview-info)
       List<@string|@number|@marker|@buffer|@file|@file-query|@window|@frame|@kmacro|@all>?)
 (@def register-preview-info-msg (info: *register-preview-info) String?)
 (@def register-preview-info-act (info: *register-preview-info) @insert|@jump|@view|@modify|@set?)
 (@def register-preview-info-smatch (info: *register-preview-info) Boolean)
 (@def register-preview-info-noconfirm (info: *register-preview-info) Bool)
 (@def register-command-info (command: Symbol) *register-preview-info?)
 (@def register-preview-forward-line (arg: Integer) Nil)
 (@def [register-preview-next register-preview-previous] () Nil)
 (@def register-type
       (register: (Cons Integer
                        (or Nil String Number Marker *registerv List<String>
                            (Cons @file String)
                            (Tuple @file-query String Integer)
                            (Cons @buffer String)
                            (Tuple WindowConfiguration Marker)
                            (Tuple (Cons @frame-configuration
                                         (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                                   Marker))))
       @string|@number|@marker|@buffer|@file|@file-query|@window|@frame|@kmacro?)
 (@def register-of-type-alist
       (types: &List<Symbol>)
       (Alist Integer
              (or Nil String Number Marker *registerv List<String>
                  (Cons @file String)
                  (Tuple @file-query String Integer)
                  (Cons @buffer String)
                  (Tuple WindowConfiguration Marker)
                  (Tuple (Cons @frame-configuration
                               (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                         Marker))))
 (@def register-preview (buffer: String|Buffer &optional show-empty: Bool) Any)
 (@variable register-preview-display-buffer-alist
            (Cons (or (fn (Args Buffer Alist<Symbol~Any>) Window?)
                      (List (fn (Args Buffer Alist<Symbol~Any>) Window?)))
                  Alist<Symbol~Any>))
 (@def register-preview-1
       (buffer: String|Buffer &optional show-empty: Bool types: &List<Symbol>?)
       Any)
 (@def register-preview-get-defaults (action: Symbol) List<String>?)
 (@def register-read-with-preview (prompt: String) Integer)
 (@def register-read-with-preview-traditional (prompt: String) Integer)
 (@def register-read-with-preview-fancy (prompt: String) Integer))

;;; ============================================================
;;; Storing locations and configurations

(et-declare
 (@def point-to-register
       (register: Integer &optional arg: Bool)
       (is-non-nil? arg
                     (Tuple (Cons @frame-configuration
                                  (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                            Marker)
                     Marker))
 (@def window-configuration-to-register
       (register: Integer &optional _arg: Any)
       (Tuple WindowConfiguration Marker))
 (@def frame-configuration-to-register
       (register: Integer &optional _arg: Any)
       (Tuple (Cons @frame-configuration
                    (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
              Marker)))

;;; ============================================================
;;; Jumping to registers

(et-declare
 (@def [register-to-point jump-to-register] (register: Integer &optional delete: Bool) Any)
 (@def register-val-jump-to
       (_val: (or Nil String Number Marker *registerv List<String>
                  (Cons @file String)
                  (Tuple @file-query String Integer)
                  (Cons @buffer String)
                  (Tuple WindowConfiguration Marker)
                  (Tuple (Cons @frame-configuration
                               (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                         Marker))
        _arg: Bool)
       Any)
 (@def register-swap-out () Nil))

;;; ============================================================
;;; Numbers and viewing registers

(et-declare
 (@def number-to-register (number: Nil|@-|Cons<Integer~Any>|Integer register: Integer) Integer)
 (@def increment-register (prefix: Nil|@-|Cons<Integer~Any>|Integer register: Integer) Any)
 (@def view-register (register: Integer) Any)
 (@def list-registers () Nil)
 (@def describe-register-1 (register: Integer &optional verbose: Bool) Any)
 (@def register-val-describe
       (val: (or Nil String Number Marker *registerv List<String>
                 (Cons @file String)
                 (Tuple @file-query String Integer)
                 (Cons @buffer String)
                 (Tuple WindowConfiguration Marker)
                 (Tuple (Cons @frame-configuration
                              (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                        Marker))
        verbose: Bool)
       Any))

;;; ============================================================
;;; Inserting and copying text

(et-declare
 (@def insert-register (register: Integer &optional arg: Bool) Any)
 (@def register-val-insert
       (_val: (or Nil String Number Marker *registerv List<String>
                  (Cons @file String)
                  (Tuple @file-query String Integer)
                  (Cons @buffer String)
                  (Tuple WindowConfiguration Marker)
                  (Tuple (Cons @frame-configuration
                               (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
                         Marker)))
       Any)
 (@def copy-to-register
       (register: Integer start: IntOrMarker end: IntOrMarker &optional delete-flag: Bool region: Bool)
       Any)
 (@def append-to-register
       (register: Integer start: IntOrMarker end: IntOrMarker &optional delete-flag: Bool)
       Any)
 (@def prepend-to-register
       (register: Integer start: IntOrMarker end: IntOrMarker &optional delete-flag: Bool)
       Any)
 (@def copy-rectangle-to-register
       (register: Integer start: IntOrMarker end: IntOrMarker &optional delete-flag: Bool)
       Any))

;;; ============================================================
