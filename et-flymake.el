;;; et-flymake.el --- Flymake backend for et.el -*- lexical-binding: t; -*-

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

(require 'find-func)
(require 'flymake)


(defvar-local et:flymake--process nil
  "The current ET Flymake process.")

(define-minor-mode et-mode
  "Typecheck the current buffer."
  :lighter nil
  (if et-mode
      (add-hook 'flymake-diagnostic-functions #'et:flymake--checker nil t)
    (remove-hook 'flymake-diagnostic-functions #'et:flymake--checker t))
  (when flymake-mode
    (flymake-start)))

(defcustom et-source-directory nil
  "Directory containing et.el source files.

When nil, `et:flymake-source-directory' finds the library providing
et.el."
  :type '(choice (const :tag "Find et.el automatically" nil)
                 directory)
  :group 'et)

(defun et:flymake-source-directory ()
  "Return the directory containing the et.el source files."
  (or et-source-directory
      (find-library-name "et")))

(defcustom et-flymake-temp-directory
  (expand-file-name "et-flymake" temporary-file-directory)
  "Directory for temporary files used by the ET Flymake backend."
  :type 'directory
  :group 'et)

(defcustom et-flymake-do-test nil
  "Whether the Flymake process should run tests."
  :type 'boolean
  :group 'et)

(defvar-local et:flymake--temp-file nil
  "Path to the temp file for the current Flymake run.")

(defun et:flymake--temp-file ()
  "Write current buffer contents to a temp file and return its path."
  (let* ((temp-dir et-flymake-temp-directory)
         (temp-file (expand-file-name (format "%s.el" (abs (random))) temp-dir)))
    (unless (file-directory-p temp-dir) (make-directory temp-dir t))
    (write-region (point-min) (point-max) temp-file nil 'silent)
    (setq-local et:flymake--temp-file temp-file)
    temp-file))

(defun et:flymake--command (true-file temp-file)
  "Return the ET batch command for TRUE-FILE and TEMP-FILE."
  (let ((source-directory (et:flymake-source-directory)))
    (list "emacs" "--batch"
          "--eval" (format "(add-to-list 'load-path %S)" source-directory)
          "--eval" "(setq load-prefer-newer t)"
          "-l" "et-cache"
          "--eval" (format "(et-process-directory %S :eval t)"
                           (expand-file-name "definitions" source-directory))
          "--eval" "(setq max-lisp-eval-depth 2400)"
          "--eval" (format "(et-flycheck-check-file %S %S :eval nil :check t :test %S)"
                           true-file temp-file et-flymake-do-test))))

(defun et:flymake--position (buffer line column)
  "Return the position in BUFFER at one-based LINE and COLUMN."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column (1- column))
        (point)))))

(defun et:flymake--parse-output (output-buffer source-buffer)
  "Parse OUTPUT-BUFFER into diagnostics for SOURCE-BUFFER."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let ((case-fold-search nil)
          diagnostics)
      (while
          (re-search-forward
           "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|hint\\|fatal\\): \\(.*\\)\\(?:\r\\)?$"
           nil t)
        (let* ((start-line (string-to-number (match-string 2)))
               (start-column (string-to-number (match-string 3)))
               (end-line (string-to-number (match-string 4)))
               (end-column (string-to-number (match-string 5)))
               (severity (match-string-no-properties 6))
               (message (match-string-no-properties 7))
               (beg (et:flymake--position
                     source-buffer start-line start-column))
               (end (et:flymake--position
                     source-buffer end-line end-column)))
          (push (flymake-make-diagnostic
                 source-buffer beg (max beg end)
                 (pcase severity
                   ((or "error" "fatal") :error)
                   ("warning" :warning)
                   ("hint" :note))
                 message)
                diagnostics)))
      (nreverse diagnostics))))


;;; Show progress

(defun et:flymake--process-filter (process output)
  "Record PROCESS OUTPUT and display ET progress messages."
  (when-let* ((output-buffer (process-buffer process))
              ((buffer-live-p output-buffer)))
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (insert output)))

  (let* ((text
          (concat
           (or (process-get process 'et:flymake--progress-remainder) "")
           output))
         (lines (split-string text "\n"))
         (complete (string-suffix-p "\n" text))
         (remainder (unless complete (car (last lines)))))
    (process-put process 'et:flymake--progress-remainder (or remainder ""))

    (dolist (line (if complete lines (butlast lines)))
      (when (string-match "\\`ET-PROGRESS:\\(.*\\)\\(?:\r\\)?\\'" line)
        (let ((progress (match-string 1 line))
              (buffer (process-get process 'et:flymake--source-buffer)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (message "ET: %s" progress)
              (redisplay t))))))))

(defun et:flymake--process-sentinel (process _event)
  "Report diagnostics when the ET Flymake PROCESS finishes."
  (unless (process-live-p process)
    (let ((source-buffer (process-get process 'et:flymake--source-buffer))
          (output-buffer (process-buffer process))
          (temp-file (process-get process 'et:flymake--temp-file))
          (report-fn (process-get process 'et:flymake--report-fn)))
      (unwind-protect
          (cond
           ((not (and (buffer-live-p source-buffer)
                      (eq process
                          (buffer-local-value 'et:flymake--process
                                              source-buffer))))
            (flymake-log :warning "ET process %s obsolete" process))
           ((zerop (process-exit-status process))
            (with-current-buffer source-buffer
              (setq et:flymake--process nil)
              (funcall report-fn
                       (et:flymake--parse-output output-buffer
                                                 source-buffer))))
           (t
            (with-current-buffer source-buffer
              (setq et:flymake--process nil)
              (funcall report-fn
                       :panic
                       :explanation
                       (format "ET process exited with status %s"
                               (process-exit-status process))))))
        (when (and temp-file (file-exists-p temp-file))
          (delete-file temp-file))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer
            (when (equal et:flymake--temp-file temp-file)
              (setq et:flymake--temp-file nil))))
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))

(defun et:flymake--checker (report-fn &rest _args)
  "Flymake backend for the ET Emacs Lisp type checker.

REPORT-FN is Flymake's diagnostic reporting callback."
  (if (not (and et-mode (derived-mode-p 'emacs-lisp-mode)))
      (funcall report-fn nil)
    (let ((old-process et:flymake--process))
      (setq et:flymake--process nil)
      (when (process-live-p old-process)
        (kill-process old-process)))

    (let* ((source-buffer (current-buffer))
           (true-file (buffer-file-name))
           (temp-file (et:flymake--temp-file))
           (output-buffer (generate-new-buffer " *et-flymake*"))
           (process
            (make-process
             :name "et-flymake"
             :buffer output-buffer
             :command (et:flymake--command true-file temp-file)
             :connection-type 'pipe
             :filter #'et:flymake--process-filter
             :sentinel #'et:flymake--process-sentinel
             :noquery t)))
      (process-put process 'et:flymake--source-buffer source-buffer)
      (process-put process 'et:flymake--temp-file temp-file)
      (process-put process 'et:flymake--report-fn report-fn)
      (setq et:flymake--process process))))


(provide 'et-flymake)
;;; et-flymake.el ends here
