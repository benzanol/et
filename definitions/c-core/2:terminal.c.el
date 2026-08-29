;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Terminal devices

(et-declare
 (@def delete-terminal (&optional terminal: Terminal|Frame? force: Bool) Nil)
 (@def frame-terminal (&optional frame: Frame?) Terminal?)
 (@def terminal-live-p (object: Terminal|Frame?) True|@x|@w32|@pc|@ns|@pgtk|@haiku|@android?)
 (@def frame-initial-p (&optional frame: Terminal|Frame?) Boolean)
 (@def terminal-list () List<Terminal>)
 (@def terminal-name (&optional terminal: Terminal|Frame?) String?))

;;; ============================================================
;;; Terminal parameters

(et-declare
 (@def terminal-parameters (&optional terminal: Terminal|Frame?) Alist<Symbol~Any>)
 (@def terminal-parameter (terminal: Terminal|Frame? parameter: Symbol) Any)
 (@def set-terminal-parameter (terminal: Terminal|Frame? parameter: Symbol value: Any) Any))

;;; ============================================================
