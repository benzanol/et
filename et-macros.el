;;; et-macros.el --- Standalone minimal macros for et.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <benzanol@nixos>
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

(defmacro et: (expr _type-spec) expr)
(defmacro et! (expr _type-spec) expr)
(defmacro et!! (expr _type-spec) expr)

(defmacro et-declare (&rest _) nil)
(defmacro et-test (&rest _) nil)

(unless (alist-get 'et defun-declarations-alist)
  (setf (alist-get 'et defun-declarations-alist) (list #'ignore)))

(unless (alist-get 'et macro-declarations-alist)
  (setf (alist-get 'et macro-declarations-alist) (list #'ignore)))


(provide 'et-macros)
;;; et-macros.el ends here
