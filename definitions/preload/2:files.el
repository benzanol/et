;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Directory abbreviation and version control

(et-declare
 (@def directory-abbrev-make-regexp (directory: String) String)
 (@def directory-abbrev-apply (filename: String) String)
 (@def version-control-safe-local-p (x: Any) Boolean))

;;; ============================================================
;;; Auto-save and lock-file minor modes

(et-declare
 (@def auto-save-visited-mode (&optional arg: Any) Boolean)
 (@def lock-file-mode (&optional arg: Any) Boolean))

;;; ============================================================
;;; Trust, file names, and directory handling

(et-declare
 (@def trusted-content-p () Boolean)
 (@def ange-ftp-completion-hook-function (op: NonNilSymbol &rest args: &List) Any)
 (@def convert-standard-filename (filename: String) String)
 (@def read-directory-name (prompt: String &optional dir: String? default-dirname: String? mustmatch: Bool initial: String?) String)
 (@def pwd (&optional insert: Bool) Nil)
 (@def parse-colon-path (search-path: String?) List<String?>)
 (@def cd-absolute (dir: String) Nil)
 (@def cd (dir: String) Nil)
 (@def directory-files-recursively
       (dir: String regexp: String &optional include-directories: Bool
        predicate: Nil|True|fn1<String> follow-symlinks: Bool)
       List<String>)
 (@def directory-empty-p (dir: String) Boolean))

;;; ============================================================
;;; Loading and locating files

(et-declare
 (@def load-file (file: String) True)
 (@def locate-file
       (filename: String path: &List<String?> &optional suffixes: &List<String>?
        predicate: Nil|Integer|NonNilSymbol|&List<NonNilSymbol>|fn1<String>)
       String?)
 (@def locate-file-completion-table
       (dirs: &List<String?> suffixes: &List<String>? string: String pred: CompletionPredicate? action: Any)
       Any)
 (@def locate-dominating-file (file: String name: String|fn1<String>) String?)
 (@def locate-user-emacs-file (new-name: String &optional old-name: String?) String)
 (@def exec-path () List<String?>)
 (@def executable-find (command: String &optional remote: Bool) String?)
 (@def load-library (library: String) True)
 (@def require-with-check (feature: NonNilSymbol &optional filename: String? noerror: Any) NonNilSymbol?))

;;; ============================================================
;;; Remote files, truenames, and symbolic links

(et-declare
 (@def file-remote-p
       (file: String &optional identification: NonNilSymbol? connected: Bool)
       Any)
 (@def file-local-name (file: String) String)
 (@def file-local-copy (file: String) String?)
 (@def file-truename (filename: String &optional counter: Any prev-dirs: Any) String)
 (@def file-chase-links (filename: String &optional limit: Integer?) String))

;;; ============================================================
;;; File sizes and temporary files

(et-declare
 (@def file-size-human-readable
       (file-size: Number &optional flavor: NonNilSymbol? space: String? unit: String?)
       String)
 (@def file-size-human-readable-iec (size: Number) String)
 (@def temporary-file-directory () String)
 (@def make-temp-file
       (prefix: String &optional dir-flag: Bool suffix: String? text: String?)
       String)
 (@def make-nearby-temp-file
       (prefix: String &optional dir-flag: Bool suffix: String?)
       String)
 (@def recode-file-name
       (file: String coding: NonNilSymbol new-coding: NonNilSymbol
        &optional ok-if-already-exists: Bool)
       String)
 (@def confirm-nonexistent-file-or-buffer () @confirm-after-completion|@confirm|Nil))

;;; ============================================================
;;; Finding and visiting files

(et-declare
 ;; FUN's literal syntax may be `(:append FUN1)', a list shape the macro
 ;; inspects unevaluated (via `car-safe') to choose append vs. prepend,
 ;; rather than a value FUN evaluates to. The checker shortcuts can only
 ;; check operands as evaluated expressions, not branch on unevaluated
 ;; list shape.
 (@check minibuffer-with-setup-hook ($todo))
 (@def find-file-read-args (prompt: String mustmatch: Any) (Tuple String True))
 (@def find-file (filename: String &optional wildcards: Bool) Buffer|List<Buffer>)
 (@def find-file-other-window (filename: String &optional wildcards: Bool) Buffer|List<Buffer>)
 (@def find-file-other-frame (filename: String &optional wildcards: Bool) Buffer|List<Buffer>)
 (@def find-file-existing (filename: String) Buffer)
 (@def find-file-read-only (filename: String &optional wildcards: Bool) Buffer|List<Buffer>)
 (@def find-file-read-only-other-window (filename: String &optional wildcards: Bool) Buffer|List<Buffer>)
 (@def find-file-read-only-other-frame (filename: String &optional wildcards: Bool) Buffer|List<Buffer>)
 (@def find-alternate-file-other-window (filename: String &optional wildcards: Bool) Any)
 (@def find-alternate-file (filename: String &optional wildcards: Bool) Any))

;;; ============================================================
;;; Visiting file buffers

(et-declare
 (@def create-file-buffer (filename: String) Buffer)
 (@def abbreviate-file-name (filename: String) String)
 (@def find-buffer-visiting (filename: String &optional predicate: fn1<Buffer~Bool>?) Buffer?)
 (@def abort-if-file-too-large
       (size: Number? op-type: String filename: String &optional offer-raw: Bool)
       Nil|@raw)
 (@def warn-maybe-out-of-memory (size: Number?) Nil)
 (@def find-file-noselect
       (filename: String &optional nowarn: Bool rawfile: Bool wildcards: Bool)
       Buffer|List<Buffer>)
 (@def find-file-noselect-1
       (buf: Buffer filename: String nowarn: Bool rawfile: Bool truename: String number: Any)
       Buffer)
 (@def insert-file-contents-literally
       (filename: String &optional visit: Bool beg: IntOrMarker? end: IntOrMarker? replace: Bool)
       (Tuple String Integer))
 (@def insert-file-1 (filename: String insert-func: fn1<String~Cons<String~Cons<Integer~Nil>>>) String?)
 (@def insert-file-literally (filename: String) String?)
 (@def find-file-literally (filename: String) Buffer)
 (@def after-find-file
       (&optional error: Bool warn: Bool noauto: Bool _after-find-file-from-revert-buffer: Any
        nomodes: Bool)
       Nil)
 ;; Obsolete alias for `with-demoted-errors', a macro (defined outside this
 ;; file) that runs BODY but demotes any error to a message and returns Nil
 ;; in that case. Accurately typing this needs the same error-demotion
 ;; result-widening the checker for `with-demoted-errors' itself needs; the
 ;; `$body' shortcut cannot express a possible Nil fallback from a caught
 ;; error.
 (@check report-errors ($todo)))

;;; ============================================================
;;; Major mode selection

(et-declare
 (@def normal-mode (&optional find-file: Bool) Any)
 (@def conf-mode-maybe () Any)
 (@def inhibit-local-variables-p () Boolean)
 (@def set-auto-mode (&optional keep-mode-if-same: Bool) Any)
 (@def major-mode-remap (mode: NonNilSymbol) NonNilSymbol|AnyFn)
 (@def set-auto-mode-0 (mode: NonNilSymbol? &optional keep-mode-if-same: Bool) Nil|:keep|NonNilSymbol)
 (@def set-auto-mode-1 () Integer?))

;;; ============================================================
;;; File local variables

(et-declare
 (@def hack-local-variables-confirm
       (all-vars: &List<&Cons<NonNilSymbol~Any>> unsafe-vars: &List<&Cons<NonNilSymbol~Any>>
        risky-vars: &List<&Cons<NonNilSymbol~Any>> dir-name: String?)
       List<Integer>?)
 (@def hack-local-variables-prop-line
       (&optional handle-mode: Any)
       Nil|NonNilSymbol|List<Cons<NonNilSymbol~Any>>)
 (@def hack-local-variables-filter
       (variables: &List<&Cons<NonNilSymbol~Any>> dir-name: String?)
       Nil)
 (@def hack-local-variables (&optional handle-mode: Any inhibit-locals: Bool) Any)
 (@def hack-local-variables-apply () Nil)
 (@def safe-local-variable-p (sym: NonNilSymbol val: Any) Any)
 (@def risky-local-variable-p (sym: NonNilSymbol &optional _ignored: Any) Any)
 (@def hack-one-local-variable-quotep (exp: Any) Boolean)
 (@def hack-one-local-variable-constantp (exp: Any) Any)
 (@def hack-one-local-variable-eval-safep (exp: Any) Any)
 (@def hack-one-local-variable (var: NonNilSymbol val: Any) Any))

;;; ============================================================
;;; Directory-local variables

(et-declare
 (@def dir-locals-get-class-variables (class: NonNilSymbol) &List<Cons<NonNilSymbol|String~Any>>)
 (@def dir-locals-collect-mode-variables
       (mode-variables: &List<Cons<NonNilSymbol~Any>> variables: &List<&Cons<NonNilSymbol~Any>>)
       List<Cons<NonNilSymbol~Any>>)
 (@def dir-locals-collect-variables
       (class-variables: &List<Cons<NonNilSymbol|String~Any>> root: String
        variables: &List<&Cons<NonNilSymbol~Any>> &optional predicate: fn1<Symbol~Bool>?)
       List<Cons<NonNilSymbol~Any>>?)
 (@def dir-locals-set-directory-class (directory: String class: NonNilSymbol &optional mtime: Any) Any)
 (@def dir-locals-set-class-variables (class: NonNilSymbol variables: [T]) T)
 (@def dir-locals-find-file (file: String) Nil|&List|String)
 (@def dir-locals-read-from-dir (dir: String) NonNilSymbol)
 (@def dir-locals-read-from-file (dir: String) NonNilSymbol)
 (@def hack-dir-local-variables () Nil)
 (@def hack-dir-local-variables-non-file-buffer () Nil))

;;; ============================================================
;;; Visiting and renaming files

(et-declare
 (@def set-visited-file-name (filename: String? &optional no-query: Bool along-with-file: Bool) Nil)
 (@def write-file (filename: String &optional confirm: Bool) Any)
 (@def rename-visited-file (new-location: String) Nil))

;;; ============================================================
;;; Backup files

(et-declare
 (@def file-extended-attributes (filename: String) List<Cons<@acl|@selinux-context~Any>>)
 (@def set-file-extended-attributes
       (filename: String attributes: &List<Cons<@acl|@selinux-context~Any>>)
       Boolean)
 (@def backup-buffer () Nil|List<Any>)
 (@def backup-buffer-copy
       (from-name: String to-name: String modes: Integer? extended-attributes: &List<Cons<Any~Any>>)
       Any)
 (@def file-name-sans-versions (name: String &optional keep-backup-version: Bool) String)
 (@def file-ownership-preserved-p (file: String &optional group: Bool) Any))

;;; ============================================================
;;; File name and backup naming

(et-declare
 (@def file-name-sans-extension (filename: String) String)
 (@def file-name-extension (filename: String &optional period: Bool) String?)
 (@def file-name-with-extension (filename: String extension: String) String)
 (@def file-name-base (&optional filename: String?) String)
 (@def file-name-split (filename: String) List<String>)
 (@def file-name-parent-directory (filename: String) String?)
 (@def normal-backup-enable-predicate (name: String) Boolean)
 (@def make-backup-file-name (file: String) String)
 (@def make-backup-file-name-1 (file: String) String)
 (@def backup-file-name-p (file: String) Integer?)
 (@def backup-extract-version (fn: String) Integer)
 (@def find-backup-file-name (fn: String) Any)
 (@def file-nlinks (filename: String) Integer?)
 (@def file-relative-name (filename: String &optional directory: String?) String))

;;; ============================================================
;;; Saving buffers

(et-declare
 (@def save-buffer (&optional arg: Integer?) Nil)
 (@def delete-auto-save-file-if-necessary (&optional force: Bool) Any)
 (@def basic-save-buffer (&optional called-interactively: Bool) Any)
 (@def basic-save-buffer-1 () Any)
 (@def basic-save-buffer-2 () Any)
 (@def save-some-buffers-root () fn)
 (@def save-some-buffers (&optional arg: Bool pred: Nil|True|fn<Nil~Bool>?) Any)
 (@def clear-visited-file-modtime () Any)
 (@def not-modified (&optional arg: Bool) Any)
 (@def insert-file (filename: String) String?)
 (@def append-to-file (start: IntOrMarker|String? end: IntOrMarker? filename: String) Any)
 (@def file-newest-backup (filename: String) String?)
 (@def file-backup-file-names (filename: String) List<String>?))

;;; ============================================================
;;; Creating and deleting files and directories

(et-declare
 (@def rename-uniquely () Any)
 (@def make-directory (dir: String &optional parents: Bool) Boolean)
 (@def make-empty-file (filename: String &optional parents: Bool) Any)
 (@def delete-file (filename: String &optional trash: Bool) Any)
 (@def delete-directory (directory: String &optional recursive: Bool trash: Bool) Any))

;;; ============================================================
;;; File comparison and predicates

(et-declare
 (@def file-equal-p (file1: String file2: String) Boolean)
 (@def file-in-directory-p (file: String dir: String) Boolean)
 (@def file-has-changed-p (file: String &optional tag: NonNilSymbol?) Nil|Cons<Integer~TimeOutput>))

;;; ============================================================
;;; Copying and pruning directories

(et-declare
 (@def copy-directory
       (directory: String newname: String &optional keep-time: Bool parents: Bool copy-contents: Bool)
       Any)
 (@def prune-directory-list (dirs: &List<String> &optional keep: &List<String>? reject: &List<String>?) List<String>))

;;; ============================================================
;;; Reverting buffers

(et-declare
 (@def revert-buffer-restore-read-only () fn?)
 (@def revert-buffer (&optional ignore-auto: Bool noconfirm: Bool preserve-modes: Bool) Any)
 (@def revert-buffer-insert-file-contents-delicately (file-name: String _auto-save-p: Any) Any)
 (@def revert-buffer-with-fine-grain (&optional ignore-auto: Bool noconfirm: Bool) Any)
 (@def revert-buffer-quick (&optional auto-save: Bool) Any))

;;; ============================================================
;;; Recovering files and sessions

(et-declare
 (@def recover-this-file () Any)
 (@def recover-file (file: String) Any)
 (@def recover-session () Any)
 (@def recover-session-finish () Any))

;;; ============================================================
;;; Killing and matching buffers

(et-declare
 (@def kill-buffer-ask (buffer: Buffer) Any)
 (@def kill-some-buffers (&optional list: &List<Buffer>?) Nil)
 (@def kill-matching-buffers (regexp: String &optional internal-too: Bool no-ask: Bool) Nil)
 (@def kill-matching-buffers-no-ask (regexp: String &optional internal-too: Bool) Nil))

;;; ============================================================
;;; Auto-save and lock files

(et-declare
 (@def rename-auto-save-file () Any)
 (@def make-auto-save-file-name () String)
 (@def make-lock-file-name (filename: String) String)
 (@def auto-save-file-name-p (filename: String) Integer?))

;;; ============================================================
;;; Wildcards and sibling files

(et-declare
 (@def wildcard-to-regexp (wildcard: String) String)
 (@def file-expand-wildcards (pattern: String &optional full: Bool regexp: Bool) List<String>)
 (@def find-sibling-file (file: String) Any)
 (@def find-sibling-file-search (file: String &optional rules: &List<Cons<String~List<String>>>?) List<String>))

;;; ============================================================
;;; Directory listings

(et-declare
 (@def list-directory (dirname: String &optional verbose: Bool) Any)
 (@def shell-quote-wildcard-pattern (pattern: String) String)
 (@def get-free-disk-space (dir: String) String?)
 (@def insert-directory-wildcard-in-dir-p (dir: String) Cons<String~String>?)
 (@def insert-directory-clean (beg: Integer switches: String|&List<String>) Any)
 (@def insert-directory
       (file: String switches: String|&List<String> &optional wildcard: Bool full-directory-p: Bool)
       Any)
 (@def insert-directory-safely
       (file: String switches: String|&List<String> &optional wildcard: Bool full-directory-p: Bool)
       Any))

;;; ============================================================
;;; Exiting Emacs

(et-declare
 (@def save-buffers-kill-emacs (&optional arg: Bool restart: Bool) Any)
 (@def save-buffers-kill-terminal (&optional arg: Bool) Any)
 (@def restart-emacs () Any))

;;; ============================================================
;;; Quoted file names

(et-declare
 ;; OPERATION selects the dispatch branch via a static table of ~30 cases,
 ;; each of which strips or re-adds "/:" quoting differently and returns a
 ;; different shape (a filename, a boolean, an arbitrary funcall result,
 ;; etc). The type language cannot express a return type that depends on
 ;; which operation symbol was passed.
 (@def file-name-non-special (operation: NonNilSymbol &rest arguments: &List) Todo)
 (@def file-name-quoted-p (name: String &optional top: Bool) Boolean)
 (@def file-name-quote (name: String &optional top: Bool) String)
 (@def file-name-unquote (name: String &optional top: Bool) String))

;;; ============================================================
;;; File modes

(et-declare
 (@def file-modes-char-to-who (char: Integer) Integer)
 (@def file-modes-char-to-right (char: Integer &optional from: Integer?) Integer)
 (@def file-modes-rights-to-number (rights: String who-mask: Integer &optional from: Integer?) Integer)
 (@def file-modes-number-to-symbolic (mode: Integer &optional filetype: Integer?) String)
 (@def file-modes-symbolic-to-number (modes: String &optional from: Integer?) Integer)
 (@def read-file-modes (&optional prompt: String? orig-file: String?) Integer))

;;; ============================================================
;;; Trash

(et-declare
 (@def move-file-to-trash (filename: String) Any))

;;; ============================================================
;;; File attributes

(et-declare
 (@def file-attribute-type (attributes: &List) Nil|True|String)
 (@def file-attribute-link-number (attributes: &List) Integer)
 (@def file-attribute-user-id (attributes: &List) String|Number)
 (@def file-attribute-group-id (attributes: &List) String|Number)
 (@def file-attribute-access-time (attributes: &List) TimeOutput)
 (@def file-attribute-modification-time (attributes: &List) TimeOutput)
 (@def file-attribute-status-change-time (attributes: &List) TimeOutput)
 (@def file-attribute-size (attributes: &List) Integer)
 (@def file-attribute-modes (attributes: &List) String)
 (@def file-attribute-inode-number (attributes: &List) Integer)
 (@def file-attribute-device-number (attributes: &List) Integer|Cons<Integer~Integer>)
 (@def file-attribute-file-identifier (attributes: &List) &List)
 ;; ATTR-NAMES are symbols used to build function names via `intern' and
 ;; `format', then funcall the corresponding accessor. The result list's
 ;; element types depend on which attribute names were passed. The type
 ;; language cannot express a return structure that depends on the runtime
 ;; values of a rest argument.
 (@def file-attribute-collect (attributes: &List &rest attr-names: &List<NonNilSymbol>) Todo))
