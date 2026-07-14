;;; et-peek.el --- Peek expression types with et.el -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'subr-x)


(defcustom et-peek-temp-directory
  (expand-file-name "et-peek" temporary-file-directory)
  "Directory for temporary files used by `et-peek'."
  :type 'directory
  :group 'et)

(defun et-peek--source-directory ()
  "Return the directory containing the et source files."
  (or (and (boundp 'et-source-directory) et-source-directory)
      (file-name-directory (or load-file-name buffer-file-name default-directory))))

(defun et-peek--bounds ()
  "Return (START END ROOT-IDX) for the expression at point."
  (save-excursion
    (condition-case nil
        (let (start end)
          (forward-sexp)
          (setq end (point))
          (backward-sexp)
          (setq start (point))
          (when-let* ((root-idx (et-peek--root-index start end)))
            (list start end root-idx)))
      (error nil))))

(defun et-peek--root-index (start end)
  "Return the root expression index containing START and END."
  (save-excursion
    (goto-char (point-min))
    (cl-loop for idx from 0
             do (forward-comment (buffer-size))
             while (< (point) (point-max))
             for root-start = (point)
             for root-end = (condition-case nil
                                (progn (forward-sexp) (point))
                              (error nil))
             when (and root-end (<= root-start start) (<= end root-end))
             return idx
             unless root-end
             return nil)))

(defun et-peek--function-position-info (start end)
  "Return (CALL-START CALL-END SYMBOL) if START..END is a function name."
  (save-excursion
    (when (eq (char-before start) ?\()
      (let ((symbol (condition-case nil
                        (read (buffer-substring-no-properties start end))
                      (error nil))))
        (when (symbolp symbol)
          (goto-char (1- start))
          (let ((call-start (point))
                (call-end (condition-case nil
                              (progn (forward-sexp) (point))
                            (error nil))))
            (when call-end
              (list call-start call-end symbol))))))))

(defun et-peek--replacement (start end id)
  "Return (REPLACE-START REPLACE-END TEXT) for peeking at START..END."
  (if-let* ((info (et-peek--function-position-info start end)))
      (pcase-let ((`(,call-start ,call-end ,symbol) info))
        (list call-start call-end
              (format "(:eval (format \"[%s] %%s\" (et-pp (et-function-type #'%S))))"
                      id symbol)))
    (list start end
          (concat "(:typeof+ "
                  (buffer-substring-no-properties start end)
                  " "
                  (prin1-to-string id)
                  ")"))))

(defun et-peek--temp-file (start end id)
  "Write a temp copy of the current buffer with START..END wrapped by ID."
  (unless (file-directory-p et-peek-temp-directory)
    (make-directory et-peek-temp-directory t))
  (let ((temp-file (make-temp-file
                    (expand-file-name "et-peek-" et-peek-temp-directory)
                    nil ".el")))
    (pcase-let ((`(,replace-start ,replace-end ,replacement)
                 (et-peek--replacement start end id)))
      (write-region
       (concat (buffer-substring-no-properties (point-min) replace-start)
               replacement
               (buffer-substring-no-properties replace-end (point-max)))
       nil temp-file nil 'silent))
    temp-file))

(defun et-peek--command (true-file temp-file root-idx)
  "Return the batch Emacs command for checking ROOT-IDX in TEMP-FILE."
  (let* ((source-directory (et-peek--source-directory))
         (definitions-directory (expand-file-name "definitions" source-directory)))
    (list "emacs" "--batch"
          "--eval" (format "(add-to-list 'load-path %S)" source-directory)
          "--eval" "(setq load-prefer-newer t)"
          "-l" "et-cache"
          "--eval" (format "(et-process-directory %S :eval t)"
                           definitions-directory)
          "--eval" (format "(et-flycheck-check-file %S %S :eval t :only-check %S)"
                           true-file temp-file root-idx))))

(defun et-peek--parse-output (output id)
  "Return the type string for ID from OUTPUT, or nil."
  (let ((prefix (concat "[" id "] ")))
    (cl-loop for line in (split-string output "\n" t)
             when (and (string-match-p ": hint: " line)
                       (string-match
                        (concat ": hint: "
                                (regexp-quote prefix)
                                "\\(.*?\\) path=")
                        line))
             return (string-trim (match-string 1 line)))))

(defun et--peek (cb)
  "Determine the type of the expression at point, then call CB with it."
  (let* ((bounds (et-peek--bounds))
         (message "Could not determine expression type"))
    (if (null bounds)
        (funcall cb message)
      (pcase-let* ((`(,start ,end ,root-idx) bounds)
                   (id (format "et-peek-%s-%s" (emacs-pid) (random)))
                   (true-file (or buffer-file-name default-directory))
                   (temp-file (et-peek--temp-file start end id))
                   (buffer (generate-new-buffer " *et-peek*"))
                   (command (et-peek--command true-file temp-file root-idx)))
        (make-process
         :name "et-peek"
         :buffer buffer
         :command command
         :noquery t
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (unwind-protect
                 (let* ((output (with-current-buffer buffer
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
                        (type (et-peek--parse-output output id)))
                   (funcall cb (or type message)))
               (when (file-exists-p temp-file)
                 (delete-file temp-file))
               (when (buffer-live-p buffer)
                 (kill-buffer buffer))))))))))

(defun et-peek ()
  "Show the type of the expression at point."
  (interactive)
  (et--peek (lambda (type) (message "%s" type))))

(provide 'et-peek)
;;; et-peek.el ends here
