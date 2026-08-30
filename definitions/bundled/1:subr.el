;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Basic Lisp macros

(et-declare
 (@check declare-function)
 (@def not ([T] arg) (is? Nil))
 (@def sxhash)
 (@check noreturn)
 (@check 1value)
 (@check def-edebug-spec)
 (@def def-edebug-elem-spec)
 (@check prog2)
 (@check setq-default)
 (@check setq-local)
 (@check defvar-local)
 (@def buffer-local-boundp)
 (@check buffer-local-set-state)
 (@def buffer-local-restore-state)
 (@check push)
 (@check pop)
 (@check static-if)
 (@check when)
 (@check unless)
 (@def subr-primitive-p)
 (@def primitive-function-p)
 (@def xor)
 (@check dolist)
 (@check dotimes)
 (@check declare)
 (@check ignore-errors)
 (@check ignore-error))


;;; ============================================================
;;; Basic Lisp functions

(et-declare
 (@def gensym)
 (@def ignore)
 (@def always)
 (@def error)
 (@def user-error)
 (@def define-error)
 (@def frame-configuration-p)
 (@def apply-partially)
 (@def zerop)
 (@def fixnump)
 (@def bignump)
 (@def lsh))


;;; ============================================================
;;; List functions

(et-declare
 (@def caar)
 (@def cadr)
 (@def cdar)
 (@def cddr)
 (@def caaar)
 (@def caadr)
 (@def cadar)
 (@def caddr)
 (@def cdaar)
 (@def cdadr)
 (@def cddar)
 (@def cdddr)
 (@def caaaar)
 (@def caaadr)
 (@def caadar)
 (@def caaddr)
 (@def cadaar)
 (@def cadadr)
 (@def caddar)
 (@def cadddr)
 (@def cdaaar)
 (@def cdaadr)
 (@def cdadar)
 (@def cdaddr)
 (@def cddaar)
 (@def cddadr)
 (@def cdddar)
 (@def cddddr)
 (@def last)
 (@def butlast)
 (@def nbutlast)
 (@def delete-dups)
 (@def delete-consecutive-dups)
 (@def number-sequence)
 (@def copy-tree))


;;; ============================================================
;;; Various list-search functions

(et-declare
 (@def assoc-default)
 (@def member-ignore-case)
 (@def assoc-delete-all)
 (@def assq-delete-all)
 (@def rassq-delete-all)
 (@def alist-get)
 (@def remove)
 (@def remq))


;;; ============================================================
;;; Keymap support

(et-declare
 (@def kbd)
 (@def undefined)
 (@def suppress-keymap)
 (@def make-composed-keymap)
 (@def define-key-after)
 (@def define-prefix-command)
 (@def map-keymap-sorted)
 (@def keymap-canonicalize)
 (@def keyboard-translate))


;;; ============================================================
;;; Key binding commands

(et-declare
 (@def global-set-key)
 (@def local-set-key)
 (@def global-unset-key)
 (@def local-unset-key)
 (@def local-key-binding)
 (@def global-key-binding))


;;; ============================================================
;;; Substitute-key-definition and its subroutines

(et-declare
 (@def substitute-key-definition)
 (@def substitute-key-definition-key))


;;; ============================================================
;;; The global keymap tree

(et-declare
 (@def ESC-prefix)
 (@def ctl-x-4-prefix)
 (@def ctl-x-5-prefix)
 (@def Control-X-prefix))


;;; ============================================================
;;; Event manipulation functions

(et-declare
 (@def listify-key-sequence)
 (@def eventp)
 (@def event-modifiers)
 (@def event-basic-type)
 (@def mouse-movement-p)
 (@def mouse-event-p)
 (@def event-start)
 (@def event-end)
 (@def event-click-count)
 (@def event-line-count))


;;; ============================================================
;;; Extracting fields of the positions in an event

(et-declare
 (@def posnp)
 (@def posn-window)
 (@def posn-area)
 (@def posn-point)
 (@def posn-set-point)
 (@def posn-x-y)
 (@def posn-col-row)
 (@def posn-actual-col-row)
 (@def posn-timestamp)
 (@def posn-string)
 (@def posn-image)
 (@def posn-object)
 (@def posn-object-x-y)
 (@def posn-object-width-height))


;;; ============================================================
;;; Obsolescent names for functions

(et-declare
 (@def log10))


;;; ============================================================
;;; Obsolescence declarations for variables, and aliases

(et-declare
 (@def compare-window-configurations)
 (@def fetch-bytecode))


;;; ============================================================
;;; Alternate names for functions - these are not being phased out

(et-declare
 (@def drop)
 (@def send-string)
 (@def send-region)
 (@def string=)
 (@def string<)
 (@def string>)
 (@def move-marker)
 (@def rplaca)
 (@def rplacd)
 (@def beep)
 (@def indent-to-column)
 (@def backward-delete-char)
 (@def search-forward-regexp)
 (@def search-backward-regexp)
 (@def int-to-string)
 (@def store-match-data)
 (@def chmod)
 (@def mkdir)
 (@def wholenump)
 (@def point-at-eol)
 (@def point-at-bol)
 (@def user-original-login-name))


;;; ============================================================
;;; Hook manipulation functions

(et-declare
 (@def add-hook)
 (@def remove-hook)
 (@check letrec)
 (@check dlet)
 (@check with-wrapper-hook)
 (@def add-to-list)
 (@def add-to-ordered-list)
 (@def add-to-history))


;;; ============================================================
;;; Mode hooks

(et-declare
 (@def run-mode-hooks)
 (@check delay-mode-hooks))


;;; ============================================================
;;; `when-let' and friends

(et-declare
 (@check if-let*)
 (@check when-let*)
 (@check and-let*)
 (@check if-let)
 (@check when-let)
 (@check while-let)
 (@def merge-ordered-lists)
 (@def derived-mode-all-parents)
 (@def provided-mode-derived-p)
 (@def derived-mode-p)
 (@def derived-mode-set-parent)
 (@def derived-mode-add-parents)
 (@def major-mode-suspend)
 (@def major-mode-restore))


;;; ============================================================
;;; Minor modes

(et-declare
 (@def add-minor-mode))


;;; ============================================================
;;; Load history

(et-declare
 (@def autoloadp)
 (@def define-symbol-prop)
 (@def locate-eln-file)
 (@def symbol-file)
 (@def locate-library))


;;; ============================================================
;;; Process stuff

(et-declare
 (@def start-process)
 (@def process-lines-handling-status)
 (@def process-lines)
 (@def process-lines-ignore-status)
 (@def process-live-p)
 (@def process-kill-buffer-query-function)
 (@def process-get)
 (@def process-put)
 (@def memory-limit))


;;; ============================================================
;;; Input and display facilities

(et-declare
 (@def read-key)
 (@def read-number)
 (@def read-char-choice)
 (@def read-char-choice-with-read-key)
 (@def sit-for)
 (@def read-char-from-minibuffer-insert-char)
 (@def read-char-from-minibuffer-insert-other)
 (@def read-char-from-minibuffer)
 (@def y-or-n-p-insert-y)
 (@def y-or-n-p-insert-n)
 (@def y-or-n-p-insert-other)
 (@def use-dialog-box-p)
 (@def y-or-n-p))


;;; ============================================================
;;; Atomic change groups

(et-declare
 (@check atomic-change-group)
 (@check with-undo-amalgamate)
 (@def prepare-change-group)
 (@def activate-change-group)
 (@def accept-change-group)
 (@def cancel-change-group))


;;; ============================================================
;;; Display-related functions

(et-declare
 (@def momentary-string-display))


;;; ============================================================
;;; Overlay operations

(et-declare
 (@def copy-overlay)
 (@def remove-overlays))


;;; ============================================================
;;; Misc. useful functions

(et-declare
 (@def buffer-narrowed-p)
 (@check with-restriction)
 (@check without-restriction)
 (@def find-tag-default-bounds)
 (@def find-tag-default)
 (@def find-tag-default-as-regexp)
 (@def find-tag-default-as-symbol-regexp)
 (@def play-sound)
 (@def shell-quote-argument)
 (@def string-to-list)
 (@def string-to-vector)
 (@def string-or-null-p)
 (@def list-of-strings-p)
 (@def booleanp)
 (@def special-form-p)
 (@def plistp)
 (@def macrop)
 (@def compiled-function-p)
 (@def integer-or-null-p)
 (@def field-at-pos)
 (@def sha1)
 (@def function-get))


;;; ============================================================
;;; Support for yanking and text properties

(et-declare
 (@def remove-yank-excluded-properties)
 (@def insert-for-yank)
 (@def insert-for-yank-1)
 (@def insert-buffer-substring-no-properties)
 (@def insert-buffer-substring-as-yank)
 (@def insert-into-buffer)
 (@def replace-string-in-region)
 (@def replace-regexp-in-region)
 (@def yank-handle-font-lock-face-property)
 (@def yank-handle-category-property))


;;; ============================================================
;;; Synchronous shell commands

(et-declare
 (@def start-process-shell-command)
 (@def start-file-process-shell-command)
 (@def call-process-shell-command)
 (@def process-file-shell-command)
 (@def call-shell-region))


;;; ============================================================
;;; Lisp macros to do various things temporarily

(et-declare
 (@check track-mouse)
 (@check with-current-buffer)
 (@def generate-new-buffer)
 (@check with-selected-window)
 (@check with-selected-frame)
 (@check save-window-excursion)
 (@def internal-temp-output-buffer-show)
 (@check with-output-to-temp-buffer)
 (@check with-temp-file)
 (@check with-temp-message)
 (@check with-temp-buffer)
 (@check with-silent-modifications)
 (@check with-output-to-string)
 (@check with-local-quit)
 (@check while-no-input)
 (@check condition-case-unless-debug)
 (@check with-demoted-errors)
 (@check combine-after-change-calls)
 (@def combine-change-calls-1)
 (@check combine-change-calls)
 (@check with-case-table)
 (@check with-file-modes)
 (@check with-existing-directory))


;;; ============================================================
;;; Matching and match data

(et-declare
 (@check save-match-data)
 (@def match-string)
 (@def match-string-no-properties)
 (@def match-substitute-replacement)
 (@def looking-back)
 (@def looking-at-p)
 (@def string-match-p)
 (@def subregexp-context-p))


;;; ============================================================
;;; Split-string

(et-declare
 (@def split-string)
 (@def string-split)
 (@def combine-and-quote-strings)
 (@def split-string-and-unquote))


;;; ============================================================
;;; Replacement in strings

(et-declare
 (@def subst-char-in-string)
 (@def string-replace)
 (@def replace-regexp-in-string)
 (@def string-equal-ignore-case)
 (@def string-prefix-p)
 (@def string-suffix-p)
 (@def bidi-string-mark-left-to-right)
 (@def string-greaterp))


;;; ============================================================
;;; Specifying things to do later

(et-declare
 (@def load-history-regexp)
 (@def load-history-filename-element)
 (@def eval-after-load)
 (@check with-eval-after-load)
 (@def do-after-load-evaluation)
 (@def display-delayed-warnings)
 (@def collapse-delayed-warnings)
 (@def delay-warning))


;;; ============================================================
;;; Invisibility specs

(et-declare
 (@def add-to-invisibility-spec)
 (@def remove-from-invisibility-spec))


;;; ============================================================
;;; Syntax tables

(et-declare
 (@check with-syntax-table)
 (@def make-syntax-table)
 (@def syntax-after)
 (@def syntax-class)
 (@def forward-word-strictly)
 (@def backward-word-strictly)
 (@def forward-whitespace)
 (@def forward-symbol)
 (@def forward-same-syntax))


;;; ============================================================
;;; Text clones

(et-declare
 (@def text-clone-create))


;;; ============================================================
;;; Mail user agents

(et-declare
 (@def define-mail-user-agent))


;;; ============================================================
;;; Unsectioned functions

(et-declare
 (@def backtrace-frames)
 (@def backtrace-frame)
 (@def called-interactively-p)
 (@def interactive-p)
 (@def internal-push-keymap)
 (@def internal-pop-keymap)
 (@def set-temporary-overlay-map)
 (@def set-transient-map))


;;; ============================================================
;;; Progress reporters

(et-declare
 (@def progress-reporter-update)
 (@def make-progress-reporter)
 (@def progress-reporter-make)
 (@def progress-reporter-force-update)
 (@def progress-reporter-do-update)
 (@def progress-reporter-done)
 (@check dotimes-with-progress-reporter)
 (@check dolist-with-progress-reporter))


;;; ============================================================
;;; Comparing version strings

(et-declare
 (@def version-to-list)
 (@def version-list-<)
 (@def version-list-=)
 (@def version-list-<=)
 (@def version-list-not-zero)
 (@def version<)
 (@def version<=)
 (@def version=))


;;; ============================================================
;;; Thread support

(et-declare
 (@check with-mutex))


;;; ============================================================
;;; Apropos

(et-declare
 (@def apropos-internal))


;;; ============================================================
;;; Misc

(et-declare
 (@def register-definition-prefixes)
 (@def flatten-tree)
 (@def flatten-list)
 (@def string-trim-left)
 (@def string-trim-right)
 (@def string-trim)
 (@def run-hook-query-error-with-timeout)
 (@def json-available-p)
 (@def ensure-list)
 (@check with-delayed-message)
 (@def function-alias-p)
 (@def readablep)
 (@def delete-line)
 (@def ensure-empty-lines)
 (@def string-lines)
 (@def buffer-match-p)
 (@def match-buffers)
 (@check handler-bind)
 (@check with-memoization))


;;; ============================================================
