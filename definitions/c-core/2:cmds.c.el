;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Motion commands

(et-declare
 (@def forward-char (&optional n: Integer?) Nil)
 (@def backward-char (&optional n: Integer?) Nil)
 (@def forward-line (&optional n: Integer?) Integer)
 (@def beginning-of-line (&optional n: Integer?) Nil)
 (@def end-of-line (&optional n: Integer?) Nil))

;;; ============================================================
;;; Editing commands

(et-declare
 (@def delete-char (n: Integer &optional killflag: Bool) Nil)
 (@def self-insert-command (n: Integer &optional c: Integer?) Nil))

;;; ============================================================
