;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Abbrev commands

(et-declare
 (@def kill-all-abbrevs () Nil)
 (@def copy-abbrev-table (table: Obarray) Obarray)
 (@def insert-abbrevs () Nil)
 (@def list-abbrevs (&optional local: Bool) Window?)
 (@def abbrev-table-name (table: Obarray) Symbol)
 (@def prepare-abbrev-list-buffer (&optional local: Bool) Buffer)
 (@def edit-abbrevs () Integer?)
 (@def edit-abbrevs-redefine () Nil)
 (@def define-abbrevs (&optional arg: Bool) Nil)
 (@def read-abbrev-file (&optional file: String? quietly: Bool) Nil)
 (@def quietly-read-abbrev-file (&optional file: String?) Nil)
 (@def write-abbrev-file (&optional file: String? verbose: Bool) Nil)
 (@def abbrev-edit-save-to-file (file: String) Nil)
 (@def abbrev-edit-save-buffer () Nil)
 (@def add-mode-abbrev (arg: Integer?) String?)
 (@def add-global-abbrev (arg: Integer?) String?)
 (@def add-abbrev (table: Obarray type: String arg: Integer?) String?)
 (@def inverse-add-mode-abbrev (n: Integer) Any)
 (@def inverse-add-global-abbrev (n: Integer) Any)
 (@def inverse-add-abbrev (table: Obarray type: String arg: Integer) Any)
 (@def abbrev-prefix-mark (&optional arg: Bool) Nil)
 (@def expand-region-abbrevs (start: IntOrMarker end: IntOrMarker &optional noquery: Bool) Nil))

;;; ============================================================
;;; Abbrev properties

(et-declare
 (@def abbrev-table-get (table: Obarray prop: Symbol) Any)
 (@def abbrev-table-put (table: Obarray prop: Symbol val: [T]) T)
 (@def abbrev-get (abbrev: Symbol prop: Symbol) Any)
 (@def abbrev-put (abbrev: Symbol prop: Symbol val: [T]) T))

;;; ============================================================
;;; Code that used to be implemented in src/abbrev.c

(et-declare
 (@def make-abbrev-table (&optional props: &List) Obarray)
 (@def abbrev-table-p (object: Any) Boolean)
 (@def abbrev-table-empty-p (object: Obarray &optional ignore-system: Bool) Boolean)
 (@def clear-abbrev-table (table: Obarray) Nil)
 (@def define-abbrev (table: Obarray abbrev: [T] expansion: Any &optional hook: Any &rest props: &List) T)
 (@def define-global-abbrev (abbrev: String expansion: Any) String)
 (@def define-mode-abbrev (abbrev: String expansion: Any) String)
 (@def abbrev-symbol (abbrev: String &optional table: Obarray|List<Obarray>?) Symbol)
 (@def abbrev-expansion (abbrev: String &optional table: Obarray|List<Obarray>?) Any)
 (@def abbrev-insert (abbrev: Symbol &optional name: String? wordstart: IntOrMarker? wordend: IntOrMarker?) Symbol)
 (@def abbrev-suggest-show-report () Window?)
 (@def expand-abbrev () Any)
 (@def unexpand-abbrev () Nil)
 (@def insert-abbrev-table-description (name: Symbol &optional readable: Bool) Nil)
 (@def define-abbrev-table (tablename: Symbol definitions: List<List<Any>> &optional docstring: String? &rest props: &List) Nil)
 (@def abbrev-table-menu (table: Obarray &optional prompt: String? sortfun: fn2<Any~Any>?) List<Any>)
 ;; This function is created by `define-derived-mode', which is not
 ;; itself part of this file's authored inventory.
 (@def edit-abbrevs-mode () Nil))

;;; ============================================================
