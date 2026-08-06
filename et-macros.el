;;; et-macros.el --- Standalone minimal macros for et.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides a very small collection of macros which are
;; designed to be used in conjunction with et.el. These macros exist
;; for annotating emacs lisp code with type annotations, to be
;; readable by et.el, and do not do any type checking on their own.


;;; Code:

(defmacro et: (_spec expr) (declare (indent 1)) expr)
(defmacro et! (_spec expr) (declare (indent 1)) expr)

(defmacro et-declare (&rest _) nil)
(defmacro et-test (&rest _) nil)

(defmacro et-defvar (symbol _type &rest rest)
  "Expands to a normal defvar, but with a place for a type annotation.

\(fn SYMBOL TYPE &optional INITVALUE DOCSTRING)"
  (declare (indent 3) (doc-string 4))
  `(defvar ,symbol . ,rest))

(defmacro et-defstruct (name &rest args)
  (declare (doc-string 2) (indent 1))
  `(cl-defstruct
       (,(or (car-safe name) name)
        (:conc-name ,(intern (format "%s->" name)))
        (:constructor ,(intern (format "%s-new" name)))
        (:copier ,(intern (format "%s-copy" name)))
        ,@(cdr-safe name))
     ,@args))

(defmacro et-defun (name arglist return &rest body)
  "Define NAME as a function with type annotations.

\(fn NAME ARGLIST RETURN [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent 3) (doc-string 4))

  (let* ((doc (when (stringp (car body)) (pop body)))
         args decls)
    (when (vectorp (car arglist)) (push (list '@generics (pop arglist)) decls))
    (while-let ((arg (pop arglist)) (arg-str (format "%s" arg)))
      (if (not (string-match "^\\(.*\\):$" arg-str)) (push arg args)
        (push (intern (match-string 1 arg-str)) args)
        (push (list (car args) (pop arglist)) decls)))
    (push (list '@return return) decls)
    (if (assq 'et-declare body) (cl-callf2 nconc (nreverse decls) (alist-get 'et-declare body))
      (cl-callf2 nconc (nreverse decls) (alist-get 'et (alist-get 'declare body))))
    `(cl-defun ,name ,(nreverse args) ,@(when doc (list doc)) ,@body)))

(setf (alist-get 'et defun-declarations-alist) (list #'ignore)
      (alist-get 'et macro-declarations-alist) (list #'ignore))

(provide 'et-macros)


;;; et-macros.el ends here
