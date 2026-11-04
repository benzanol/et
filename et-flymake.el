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

(require 'cl-lib)
(require 'et-cache)
(require 'find-func)
(require 'flymake)
(require 'subr-x)


;;; ============================================================
;;; Globals

(defcustom et-source-directory nil
  "Directory containing et.el source files.

When nil, `et:flymake-source-directory' finds the library providing
et.el."
  :type '(choice (const :tag "Find et.el automatically" nil)
                 directory)
  :group 'et)

(defcustom et-flymake-temp-directory
  (expand-file-name "et-flymake" temporary-file-directory)
  "Directory for temporary files used by the ET Flymake backend."
  :type 'directory
  :group 'et)

(defcustom et-flymake-do-test nil
  "Whether the Flymake process should run tests."
  :type 'boolean
  :group 'et)

(defvar-local et:flymake--process nil
  "The current ET Flymake process.")

(defvar-local et:flymake--temp-file nil
  "Path to the temp file for the current Flymake run.")


;;; ============================================================
;;; Mode

(define-minor-mode et-mode
  "Typecheck the current buffer."
  :lighter nil
  (if et-mode
      (progn
        (add-hook 'flymake-diagnostic-functions #'et:flymake--checker nil t)
        (flymake-mode 1)
        (flymake-start nil t))
    (remove-hook 'flymake-diagnostic-functions #'et:flymake--checker t)
    (when flymake-mode
      (flymake-start))))


;;; ============================================================
;;; Flymake
;;;; Checker command

(defun et:flymake-source-directory ()
  "Return the directory containing the et.el source files."
  (or et-source-directory
      (when-let* ((library
                   (condition-case nil
                       (find-library-name "et")
                     (file-error nil))))
        (file-name-directory library))
      (error "Could not find the directory providing et.el")))

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


;;;; Diagnostics

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


;;;; Process handling

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


;;;; Flymake backend

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


;;; ============================================================
;;; Peeking

(defvar-local et:peek--process nil
  "The current ET peeking process.")

(defun et:peek--bounds ()
  "Return the bounds of the expression at point."
  (save-restriction
    (widen)
    (save-excursion
      (condition-case nil
          (let (start end)
            (forward-sexp)
            (setq end (point))
            (backward-sexp)
            (setq start (point))
            (cons start end))
        (error nil)))))

(defun et:peek--root-index (start end)
  "Return the index of the top-level expression containing START through END."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (cl-loop for index from 0
               do (forward-comment (buffer-size))
               while (< (point) (point-max))
               for root-start = (point)
               for root-end = (condition-case nil
                                  (progn (forward-sexp) (point))
                                (error nil))
               when (and root-end
                         (<= root-start start)
                         (<= end root-end))
               return index
               unless root-end
               return nil))))

(defun et:peek--temp-file (source-buffer start end id)
  "Copy SOURCE-BUFFER to a temp file and wrap START through END with ID."
  (unless (file-directory-p et-flymake-temp-directory)
    (make-directory et-flymake-temp-directory t))
  (let ((contents
         (with-current-buffer source-buffer
           (save-restriction
             (widen)
             (buffer-substring-no-properties (point-min) (point-max)))))
        (temp-file
         (make-temp-file
          (expand-file-name "et-peek-" et-flymake-temp-directory)
          nil ".el")))
    (with-temp-buffer
      (insert contents)
      (goto-char end)
      (insert " " (prin1-to-string id) ")")
      (goto-char start)
      (insert "(:typeof ")
      (write-region (point-min) (point-max) temp-file nil 'silent))
    temp-file))

(defun et:peek--command (true-file temp-file root-index)
  "Return the command for peeking in TRUE-FILE using TEMP-FILE.

Only the top-level expression at ROOT-INDEX is type-checked."
  (let ((source-directory (et:flymake-source-directory)))
    (list "emacs" "--batch"
          "--eval" (format "(add-to-list 'load-path %S)" source-directory)
          "--eval" "(setq load-prefer-newer t)"
          "-l" "et-cache"
          "--eval" (format "(et-process-directory %S :eval t)"
                           (expand-file-name "definitions" source-directory))
          "--eval" "(setq max-lisp-eval-depth 2400)"
          "--eval" (format "(et-flycheck-check-file %S %S :eval nil :only-check %S)"
                           true-file temp-file root-index))))

(defun et:peek--parse-output (output-buffer id)
  "Return the type marked with ID in OUTPUT-BUFFER, or nil."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let ((regexp
           (concat
            ": hint: "
            "\\(?:\\[[^]\r\n]*\\] \\)*"
            "\\[" (regexp-quote id) "\\] "
            "\\([^\r\n]*\\)\\(?:\r\\)?$"))
          types)
      (while (re-search-forward regexp nil t)
        (push (string-trim (match-string-no-properties 1)) types))
      (when types
        (string-join (nreverse types) "\n")))))

(defun et:peek--process-sentinel (process _event)
  "Report the type when the peeking PROCESS finishes."
  (unless (process-live-p process)
    (let ((source-buffer (process-get process 'et:peek--source-buffer))
          (output-buffer (process-buffer process))
          (temp-file (process-get process 'et:peek--temp-file))
          (id (process-get process 'et:peek--id))
          (callback (process-get process 'et:peek--callback)))
      (unwind-protect
          (when (and (buffer-live-p source-buffer)
                     (eq process
                         (buffer-local-value 'et:peek--process source-buffer)))
            (with-current-buffer source-buffer
              (setq et:peek--process nil)
              (funcall
               callback
               (or (and (zerop (process-exit-status process))
                        (et:peek--parse-output output-buffer id))
                   "Could not determine expression type"))))
        (when (and temp-file (file-exists-p temp-file))
          (delete-file temp-file))
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))

(defun et-infer-at-point (callback)
  "Call CALLBACK with the type of the expression at point."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (pcase-let* ((`(,start . ,end)
                (or (et:peek--bounds)
                    (user-error "No expression at point")))
               (root-index
                (or (et:peek--root-index start end)
                    (user-error "Expression is not inside a top-level form")))
               (source-buffer (current-buffer))
               (id (format "et-peek-%s-%s" (emacs-pid) (abs (random))))
               (temp-file (et:peek--temp-file
                           source-buffer start end id))
               (output-buffer (generate-new-buffer " *et-peek*"))
               (old-process et:peek--process))
    (setq et:peek--process nil)
    (when (process-live-p old-process)
      (kill-process old-process))
    (condition-case err
        (let ((process
               (make-process
                :name "et-peek"
                :buffer output-buffer
                :command (et:peek--command
                          buffer-file-name temp-file root-index)
                :connection-type 'pipe
                :sentinel #'et:peek--process-sentinel
                :noquery t)))
          (process-put process 'et:peek--source-buffer source-buffer)
          (process-put process 'et:peek--temp-file temp-file)
          (process-put process 'et:peek--id id)
          (process-put process 'et:peek--callback callback)
          (setq et:peek--process process))
      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (when (buffer-live-p output-buffer)
         (kill-buffer output-buffer))
       (signal (car err) (cdr err))))))

(defun et-peek ()
  "Show the type of the expression at point in the minibuffer."
  (interactive)
  (et-infer-at-point (lambda (result) (message "%s" result))))


;;; ============================================================
;;; Provide

(provide 'et-flymake)
;;; et-flymake.el ends here
