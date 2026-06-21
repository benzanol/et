;;; et-cache.el --- Caching for et.el -*- lexical-binding: t; -*-

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
;;; Code:

(require 'et-check)


;;; ============================================================
;;; Utils
;;;; Recursive copy

(defun et--recursive-copy (object func)
  "Return a copy of OBJECT by applying FUNC to every object.

This will start by applying FUNC to OBJECT. If the returned
result is an atom (not a cons cell,) it is returned. Otherwise,
what will be returned is the result of repeating this process for
both sides of the cons cell."
  (declare (et (object Any)
               (func Function<Any~Any>)
               (@return Any)
               (@skip)))

  (if (not (eq object (setq object (funcall func object))))
      object

    (cond
     ;; Handle structs
     ((recordp object)
      (let* ((entries nil))
        (dotimes (i (length object))
          (push (et--recursive-copy (aref object i) func) entries))
        (apply #'record (nreverse entries))))

     ((atom object) object)
     ;; We could just do (cons (copy (car object) func) (copy (cdr object) func)),
     ;; but this would hit the recursion limit for long lists.
     ;; The current solution is equivalent, but does all elements of a list in the same call.
     ((prog1 (setq object (cons (et--recursive-copy (car object) func)
                                (funcall func (cdr object))))
        (while (consp (cdr object))
          (setcdr object (cons (et--recursive-copy (cadr object) func)
                               (funcall func (cddr object))))
          (setq object (cdr object))))))))


;;;; Substitute with placeholder

(defun et--subst-placeholder (idx)
  (declare (et (idx Integer)
               (@return Symbol)))
  (intern (format "@@et-ph-%s@@" idx)))

(defun et--subst-to-placeholders (object pred)
  "Substitute certain values in OBJECT with placeholders.

This will return (NEW-OBJECT REPL-LIST) where NEW-OBJECT is OBJECT with
all values matching PRED replaced by a placeholder. Values that are `eq'
will get replaced by the same placeholder. REPL-ALIST is a list
of (VALUE . REPLACEMENT) that were replaced by placeholders."
  (let* ((repl-alist nil))
    (cons
     (et--recursive-copy
      object
      (lambda (x)
        (if (not (funcall pred x)) x
          (or (alist-get x repl-alist)
              (cdar (push (cons x (et--subst-placeholder (length repl-alist))) repl-alist))))))
     repl-alist)))

(defun et--subst-with-placeholders (object repl-alist)
  (et--recursive-copy object (lambda (x) (or (alist-get x repl-alist) x))))

(defun et--subst-from-placeholders (object repl-alist)
  (et--recursive-copy
   object
   (lambda (x) (if-let* ((entry (rassq x repl-alist)))
                   (car entry)
                 x))))

(et-test
 (pcase-let* ((`(,new-obj . ,repls)
               (et--subst-to-placeholders
                '((1 2 3) (4 5 6))
                (lambda (x) (and (numberp x) (= 0 (mod x 2)))))))
   (and (pcase new-obj (`((1 ,(pred symbolp) 3) (,(pred symbolp) 5 ,(pred symbolp))) t))
        (= 3 (length repls))
        (equal (et--subst-from-placeholders (list (cadr new-obj) (car new-obj)) repls)
               '((4 5 6) (1 2 3))))))


;;; ============================================================
;;; Caching
;;;; Variables

(defvar et-cache-file "~/.emacs.d/.cache/et-cache.el"
  "File to use to store the cache.")

(defvar et-cache nil
  "Hash table containing the cache.")

(defvar et--caching nil
  "Non-nil when the expression currently being evaluated will be cached.")

(defvar et-cache-hits 'NO
  "When this is a list, cache hits will be placed here for debugging.")

(defvar et-caching-enabled t
  "Whether to enable caching.")


;;;; Disk operations

(defun et--load-cache ()
  (let* ((hash-table (make-hash-table :test #'equal)))
    ;; Read the file into a hashtable
    (when (and (stringp et-cache-file) (file-exists-p et-cache-file))
      (with-temp-buffer
        (insert-file-contents et-cache-file)
        (goto-char (point-min))
        (while (let* ((pair (ignore-errors (read (current-buffer)))))
                 (when (consp pair)
                   (puthash (car pair) (cdr pair) hash-table)
                   t)))))
    hash-table))

(defun et--cache-retrieve (key)
  (ignore-errors
    (when-let* ((val (gethash key (if (hash-table-p et-cache) et-cache
                                    (setq et-cache (et--load-cache))))))
      ;; For debugging
      (when (listp et-cache-hits)
        (push (cons key val) et-cache-hits))

      ;; Return a copy
      (et--recursive-copy val #'identity))))

(defun et--cache-store (key value)
  (ignore-errors
    (unless (hash-table-p et-cache) (setq et-cache (et--load-cache)))

    (puthash key value et-cache)
    (write-region (format "%s" (prin1-to-string (cons key value))) nil et-cache-file
                  (file-exists-p et-cache-file)))
  value)


;;;; Macros

(defmacro et-cache (key ph-pred &rest body)
  (declare (indent 2))
  `(pcase-let* ((et--caching t)
                (`(,subst-key . ,repls) (et--subst-to-placeholders ,key ,ph-pred)))
     (if-let* ((_ et-caching-enabled)
               (retrieved (et--cache-retrieve subst-key)))
         (et--subst-from-placeholders retrieved repls) ; Insert placeholders

       (let* ((value (progn . ,body)))
         (et--cache-store subst-key (et--subst-with-placeholders value repls))
         value))))

(defmacro et-cache-if (bool key ph-pred &rest body)
  (declare (indent 3))
  `(if ,bool
       (et-cache ,key ,ph-pred ,@body)
     ,@body))


;;;; Bonus functions

(defun et-clear-cache ()
  (interactive)
  (setq et-cache nil)
  (write-region "" nil et-cache-file))

(defun et-refresh-cache ()
  (interactive)
  (setq et-cache (et--load-cache)))
