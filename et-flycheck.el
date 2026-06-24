;;; et-flycheck.el --- Flycheck checker for et.el -*- lexical-binding: t; -*-

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

(require 'flycheck)


(define-minor-mode et-mode
  "Typecheck the current buffer.")

(defcustom et-source-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing et.el source files."
  :type 'directory
  :group 'et)

(defcustom et-flycheck-temp-directory
  (expand-file-name "et-flycheck" temporary-file-directory)
  "Directory for temporary files used by the et flycheck checker."
  :type 'directory
  :group 'et)

(defcustom et-flycheck-do-test nil
  "Whether the flycheck process should run tests."
  :type 'boolean
  :group 'et)

(defvar-local et--flycheck-temp-file nil
  "Path to the temp file for the current flycheck run.")

(defun et--flycheck-temp-file ()
  "Write current buffer contents to a temp file and return its path."
  (let* ((temp-dir et-flycheck-temp-directory)
         (temp-file (expand-file-name (format "%s.el" (abs (random))) temp-dir)))
    (unless (file-directory-p temp-dir) (make-directory temp-dir t))
    (write-region (point-min) (point-max) temp-file nil 'silent)
    (setq-local et--flycheck-temp-file temp-file)
    temp-file))

(defun et-flycheck--error-filter (errors)
  ;; Delete the temp file
  (when (and et--flycheck-temp-file (file-exists-p et--flycheck-temp-file))
    (delete-file et--flycheck-temp-file))
  errors)

(flycheck-define-checker et-flycheck-checker
  "Type checker for Emacs Lisp using et.el."
  :command ("emacs" "--batch"
            "--eval" (eval (format "(add-to-list 'load-path %S)" et-source-directory))
            "--eval" "(setq load-prefer-newer t)"
            "-l" "et-cache"
            "--eval" (eval (format "(et-process-directory %S :eval t)"
                                   (expand-file-name "definitions" et-source-directory)))
            "--eval" (eval (format "(et-flycheck-check-file %S %S :check t :test %S)"
                                   (buffer-file-name) (et--flycheck-temp-file)
                                   et-flycheck-do-test)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" end-line ":" end-column ": error: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":" end-line ":" end-column ": warning: " (message) line-end)
   (info line-start (file-name) ":" line ":" column ":" end-line ":" end-column ": hint: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ":" end-line ":" end-column ": fatal: " (message) line-end))
  :error-filter et-flycheck--error-filter
  :modes (emacs-lisp-mode)
  :predicate (lambda () (bound-and-true-p et-mode))
  :next-checkers nil
  ;; ((t . emacs-lisp) (t . emacs-lisp-checkdoc))
  )

(add-to-list 'flycheck-checkers 'et-flycheck-checker)


(provide 'et-flycheck)
;;; et-flycheck.el ends here
