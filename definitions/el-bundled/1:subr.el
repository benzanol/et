;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Basic Lisp macros

(et-declare
 (@check declare-function)
 (@def not (arg: [T]) (is? T Nil))
 (@def sxhash (object: Any) Integer)
 (@check noreturn ($fn Any Never))
 (@check 1value)
 (@check def-edebug-spec)
 (@def def-edebug-elem-spec
       ([(<= S &List)] name: Symbol spec: S)
       S)
 (@check lambda)
 (@check prog2)
 (@check setq-default)
 (@check setq-local)
 (@check defvar-local)
 (@def buffer-local-boundp (symbol: Symbol buffer: Buffer) Boolean)
 (@check buffer-local-set-state)
 (@def buffer-local-restore-state)
 (@check push
         ($arglist (newelt place)
                   ($exp `(setf ,place (cons ,newelt ,place)))))
 (@check pop
         ($arglist (place)
                   ($bind [car ($exp `(car ,place))]
                          ($exp `(setf ,place (cdr ,place)))
                          ($var car))))
 (@check static-if)
 (@check when
         ($pcase
          `(,_condition . ,_body) ($if ($at 1) ($tail 2) ($type Nil))))
 (@check unless
         ($pcase
          `(,_condition . ,_body) ($if ($at 1) ($type Nil) ($tail 2))))
 (@def subr-primitive-p (object: Any) Boolean)
 (@def primitive-function-p (object: Any) Boolean)
 (@def xor ([A B] cond1: A cond2: B) A|B|Nil)
 (@check dolist
         ($pcase
          `((,(and var (pred symbolp)) ,_list) . ,_body)
          ($bind [elem ($infer [T] &List<T> T ($at 1 1))]
                 ($progn
                  ($bind [_ var ($var elem)] ($loop ($tail 2)))
                  ($type Nil)))
          `((,(and var (pred symbolp)) ,_list ,_result) . ,_body)
          ($bind [elem ($infer [T] &List<T> T ($at 1 1))]
                 ($progn
                  ($bind [_ var ($var elem)] ($loop ($tail 2)))
                  ($at 1 2)))))
 (@check dotimes
         ($pcase
          `((,(and var (pred symbolp)) ,_count) . ,_body)
          ($progn
           ($expect ($type Integer) ($at 1 1))
           ($bind [_ var ($type Integer)] ($loop ($tail 2)))
           ($type Nil))
          `((,(and var (pred symbolp)) ,_count ,_result) . ,_body)
          ($progn
           ($expect ($type Integer) ($at 1 1))
           ($bind [_ var ($type Integer)] ($loop ($tail 2)))
           ($bind [_ var ($type Integer)] ($at 1 2)))))
 (@check declare ($type Nil))
 (@check ignore-errors
         ($branches ($tail 1) ($type Nil)))
 (@check ignore-error))


;;; ============================================================
;;; Basic Lisp functions

(et-declare
 (@def gensym (&optional prefix: String?) Var)
 (@def ignore (&rest arguments: &List) Nil)
 (@def always (&rest arguments: &List) True)
 (@def error (format: String &rest arguments: &List) Never)
 (@def user-error (format: String &rest arguments: &List) Never)
 (@def define-error
       (name: Symbol message: String &optional parent: Symbol|&List<Symbol>?)
       String)
 (@def frame-configuration-p (object: Any) Boolean)
 (@def apply-partially (fun: AnyFn &rest args: &List) (fn &List Any))
 (@def zerop (number: Number) Boolean)
 (@def fixnump (object: Any) Boolean)
 (@def bignump (object: Any) Boolean)
 (@def lsh (value: Integer count: Integer) Integer))


;;; ============================================================
;;; List functions

(et-declare
 (@def caar ([T] list: &List<&Cons<T~Any>>) T?)
 (@def cadr ([T] list: &List<T>) T?)
 (@def cdar ([T] list: &List<&Cons<Any~T>>) T?)
 (@def cddr ([T] list: &List<T>) &List<T>)
 (@def caaar ([T] list: &List<&List<&List<T>>>) T?)
 (@def caadr ([T] list: &List<&Cons<T~Any>>) T?)
 (@def cadar ([T] list: &List<&List<T>>) T?)
 (@def caddr ([T] list: &List<T>) T?)
 (@def cdaar ([T] list: &List<&List<&List<T>>>) &List<T>)
 (@def cdadr ([T] list: &List<&List<T>>) &List<T>)
 (@def cddar ([T] list: &List<&List<T>>) &List<T>)
 (@def cdddr ([T] list: &List<T>) &List<T>)
 (@def caaaar ([T] list: &List<&List<&List<&List<T>>>>) T?)
 (@def caaadr ([T] list: &List<&List<&List<T>>>) T?)
 (@def caadar ([T] list: &List<&List<&List<T>>>) T?)
 (@def caaddr ([T] list: &List<&List<T>>) T?)
 (@def cadaar ([T] list: &List<&List<&List<T>>>) T?)
 (@def cadadr ([T] list: &List<&List<T>>) T?)
 (@def caddar ([T] list: &List<&List<T>>) T?)
 (@def cadddr ([T] list: &List<T>) T?)
 (@def cdaaar ([T] list: &List<&List<&List<&List<T>>>>) &List<T>)
 (@def cdaadr ([T] list: &List<&List<&List<T>>>) &List<T>)
 (@def cdadar ([T] list: &List<&List<&List<T>>>) &List<T>)
 (@def cdaddr ([T] list: &List<&List<T>>) &List<T>)
 (@def cddaar ([T] list: &List<&List<&List<T>>>) &List<T>)
 (@def cddadr ([T] list: &List<&List<T>>) &List<T>)
 (@def cdddar ([T] list: &List<&List<T>>) &List<T>)
 (@def cddddr ([T] list: &List<T>) &List<T>)
 (@def last ([T] list: &List<T> &optional n: Integer?) &List<T>)
 (@def butlast ([T] list: &List<T> &optional n: Integer?) &List<T>)
 (@def nbutlast ([T] list: List<T> &optional n: Integer?) List<T>)
 (@def delete-dups ([T] list: List<T>) List<T>)
 (@def delete-consecutive-dups
       ([T] list: List<T> &optional circular: Bool)
       List<T>)
 (@def number-sequence
       (from: Number &optional to: Number? inc: Number?)
       ListFresh<Number>)
 (@def copy-tree ([T] tree: T &optional vectors-and-records: Bool) (freshen-deep T)))


;;; ============================================================
;;; Various list-search functions

(et-declare
 (@def assoc-default)
 (@def member-ignore-case ([T] element: String list: &List<T>) &List<T>)
 (@def assoc-delete-all)
 (@def assq-delete-all ([K V] key: K alist: Alist<K~V>) Alist<K~V>)
 (@def rassq-delete-all ([K V] value: V alist: Alist<K~V>) Alist<K~V>)
 (@def alist-get
       ([K V] key: K alist: &Alist<K~V>
        &optional default: V remove: Bool testfn: (or Nil (fn (Args K K) Any)))
       V?)
 (@def remove
       ([E] element: Any sequence: [<= S &OrdSeq<E>])
       (freshen-shallow S))
 (@def remq ([T] element: Any list: &List<T>) &List<T>))


;;; ============================================================
;;; Keymap support

(et-declare
 (@def kbd (keys: String) String|Vector<Any>)
 (@def undefined () Any)
 (@def suppress-keymap
       (map: Cons<@keymap~Any>|Symbol &optional nodigits: [N])
       N|Nil)
 (@def make-composed-keymap
       (maps: &Cons<@keymap~Any>|Symbol|&List<&Cons<@keymap~Any>|Symbol>
              &optional parent: &Cons<@keymap~Any>|Symbol?)
       ConsFresh<@keymap~Any>)
 (@def define-key-after
       (keymap: Cons<@keymap~Any>|Symbol key: String|Vector<Any>
                definition: Any &optional after: Integer|Symbol)
       Nil)
 (@def define-prefix-command
       ([(<= C Symbol)] command: C &optional mapvar: Symbol? name: String?)
       C)
 (@def map-keymap-sorted
       (function: (fn (Args Any Any) Any)
                  keymap: &Cons<@keymap~Any>|Symbol)
       Nil)
 (@def keymap-canonicalize
       (map: &Cons<@keymap~Any>|Symbol)
       ConsFresh<@keymap~Any>)
 (@def keyboard-translate (from: Integer to: Integer) Integer))


;;; ============================================================
;;; Key binding commands

(et-declare
 (@def global-set-key (key: String|Vector<Any> command: [C]) C|Nil)
 (@def local-set-key (key: String|Vector<Any> command: [C]) C|Nil)
 (@def global-unset-key (key: String|Vector<Any>) Nil)
 (@def local-unset-key (key: String|Vector<Any>) Nil)
 (@def local-key-binding
       (keys: String|&Vector<Any> &optional accept-default: Bool)
       Any)
 (@def global-key-binding
       (keys: String|&Vector<Any> &optional accept-default: Bool)
       Any))


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
 (@def listify-key-sequence
       (key: String|&Vector<Any>)
       ListFresh<Any>)
 (@def eventp (object: Any) Boolean)
 (@def event-modifiers (event: Any) &List<Symbol>)
 (@def event-basic-type (event: Any) Integer|Symbol)
 (@def mouse-movement-p (object: Any) Boolean)
 (@def mouse-event-p (object: Any) Nil|&List<Symbol>)
 (@def event-start (event: Any) &List)
 (@def event-end (event: Any) &List)
 (@def event-click-count (event: Any) Integer)
 (@def event-line-count (event: Any) Integer))


;;; ============================================================
;;; Extracting fields of the positions in an event

(et-declare
 (@def posnp (object: Any) Boolean)
 (@def posn-window (position: &List) Window|Frame?)
 (@def posn-area (position: &List) Symbol)
 (@def posn-point (position: &List) IntOrMarker?)
 (@def posn-set-point (position: &List) Integer?)
 (@def posn-x-y (position: &List) &Cons<Integer~Integer>)
 (@def posn-col-row
       (position: &List &optional use-window: Bool)
       ConsFresh<Integer~Integer>)
 (@def posn-actual-col-row
       (position: &List)
       &Cons<Integer~Integer>?)
 (@def posn-timestamp (position: &List) Integer?)
 (@def posn-string (position: &List) &Cons<String~Integer>?)
 (@def posn-image (position: &List) &List?)
 (@def posn-object (position: &List) &List|&Cons<String~Integer>?)
 (@def posn-object-x-y (position: &List) &Cons<Integer~Integer>?)
 (@def posn-object-width-height
       (position: &List)
       &Cons<Integer~Integer>?))


;;; ============================================================
;;; Obsolescent names for functions

(et-declare
 (@def log10 (x: Number) Number))


;;; ============================================================
;;; Obsolescence declarations for variables, and aliases

(et-declare
 (@def compare-window-configurations
       (configuration-1: WindowConfiguration
                         configuration-2: WindowConfiguration)
       Boolean)
 (@def fetch-bytecode (&rest arguments: &List) Nil))


;;; ============================================================
;;; Alternate names for functions - these are not being phased out

(et-declare
 (@def drop ([E] n: Integer list: &List<E>) &List<E>)
 (@def send-string (process: Process|Buffer|String? string: String) Nil)
 (@def send-region
       (process: Process|Buffer|String? start: IntOrMarker end: IntOrMarker)
       Nil)
 (@def string= (string-1: Symbol|String string-2: Symbol|String) Boolean)
 (@def string< (string-1: Symbol|String string-2: Symbol|String) Boolean)
 (@def string> (string-1: Symbol|String string-2: Symbol|String) Boolean)
 (@def move-marker
       ([(<= M Marker)] marker: M position: IntOrMarker?
        &optional buffer: Buffer?)
       M)
 (@def rplaca ([A B] cell: Cons<A~B> newcar: A) A)
 (@def rplacd ([A B] cell: Cons<A~B> newcdr: B) B)
 (@def beep (&optional arg: Bool) Nil)
 (@def indent-to-column (column: Integer &optional minimum: Integer?) Integer)
 (@def backward-delete-char (n: Integer &optional killflag: Bool) Nil)
 (@def search-forward-regexp
       (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def search-backward-regexp
       (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def int-to-string (number: Number) String)
 (@def store-match-data (list: List &optional reseat: Bool) Nil)
 (@def chmod (filename: String mode: Integer &optional flag: @nofollow?) Nil)
 (@def mkdir (directory: String &optional parents: Bool) Boolean)
 (@def wholenump ([T] object: T) (is-a? T Integer))
 (@def point-at-eol (&optional n: Integer?) Integer)
 (@def point-at-bol (&optional n: Integer?) Integer)
 (@def user-original-login-name (&optional uid: Integer?) String?))


;;; ============================================================
;;; Hook manipulation functions

(et-declare
 (@def add-hook
       (hook: Symbol function: AnyFn
              &optional depth: Number|Bool local: Bool)
       Any)
 (@def remove-hook
       (hook: Symbol function: AnyFn &optional local: Bool)
       Any)
 (@check letrec)
 (@check dlet)
 (@check with-wrapper-hook)
 (@def add-to-list)
 (@def add-to-ordered-list
       (list-var: Symbol element: Any &optional order: Number|Bool)
       &List)
 (@def add-to-history
       (history-var: Symbol newelt: Any
                     &optional maxelt: Integer|Symbol? keep-all: Bool)
       &List?))


;;; ============================================================
;;; Mode hooks

(et-declare
 (@def run-mode-hooks (&rest hooks: &List<Symbol>) Nil)
 (@check delay-mode-hooks ($body)))


;;; ============================================================
;;; `when-let' and friends

(et-declare
 (@check if-let* ($expand))
 (@check when-let* ($expand))
 (@check and-let*)
 (@check if-let)
 (@check when-let)
 (@check while-let)
 (@def merge-ordered-lists
       ([T] lists: &List<&List<T>>
        &optional error-function: fn1<&List<&List<T>>~T>?)
       &List<T>)
 (@def derived-mode-all-parents
       (mode: Symbol &optional known-children: &List<Symbol>?)
       &List<Symbol>)
 (@def provided-mode-derived-p
       (mode: Symbol &optional modes: Symbol|&List<Symbol>
              &rest old-modes: &List<Symbol>)
       Symbol)
 (@def derived-mode-p
       (&optional modes: Symbol|&List<Symbol>
                  &rest old-modes: &List<Symbol>)
       Symbol)
 (@def derived-mode-set-parent (mode: Symbol parent: Symbol) Nil)
 (@def derived-mode-add-parents
       (mode: Symbol extra-parents: &List<Symbol>)
       Nil)
 (@def major-mode-suspend () Symbol)
 (@def major-mode-restore))


;;; ============================================================
;;; Minor modes

(et-declare
 (@def add-minor-mode
       (toggle: Symbol name: String|Symbol?
                &optional keymap: &Cons<@keymap~Any>|Symbol?
                after: Symbol? toggle-fun: AnyFn?)
       Nil))


;;; ============================================================
;;; Load history

(et-declare
 (@def autoloadp
       ([T] object: T)
       (is-a? T Cons<@autoload~Any>))
 (@def define-symbol-prop
       ([T] symbol: Symbol prop: Symbol val: T)
       T)
 (@def locate-eln-file (eln-file: String) String?)
 (@def symbol-file
       (symbol: Symbol &optional type: Symbol native-p: Bool)
       String?)
 (@def locate-library
       (library: String &optional nosuffix: Bool path: &List<String>
                 interactive-call: Bool)
       String?))


;;; ============================================================
;;; Process stuff

(et-declare
 (@def start-process
       (name: String buffer: Buffer|String? program: String?
              &rest program-args: &List<String>)
       Process)
 (@def process-lines-handling-status
       (program: String status-handler: fn1<Integer|String>?
                 &rest args: &List<String>)
       ListFresh<String>)
 (@def process-lines (program: String &rest args: &List<String>) ListFresh<String>)
 (@def process-lines-ignore-status
       (program: String &rest args: &List<String>)
       ListFresh<String>)
 (@def process-live-p (process: Any) Nil|&List<Symbol>)
 (@def process-kill-buffer-query-function () Boolean)
 (@def process-get (process: Process propname: Symbol) Any)
 (@def process-put
       (process: Process propname: Symbol value: [V])
       V)
 (@def memory-limit () Integer))


;;; ============================================================
;;; Input and display facilities

(et-declare
 (@def read-key (&optional prompt: String? disable-fallbacks: Bool) Any)
 (@def read-number
       (prompt: String &optional default: Number|&List<Number>? hist: Symbol?)
       Number)
 (@def read-char-choice
       (prompt: String chars: &List<Integer>
                &optional inhibit-keyboard-quit: Bool)
       Integer)
 (@def read-char-choice-with-read-key
       (prompt: String chars: &List<Integer>
                &optional inhibit-keyboard-quit: Bool)
       Integer)
 (@def sit-for (seconds: Number &optional nodisp: Bool) Boolean)
 (@def read-char-from-minibuffer-insert-char () Nil)
 (@def read-char-from-minibuffer-insert-other () Nil)
 (@def read-char-from-minibuffer
       (prompt: String &optional chars: &List<Integer>? history: Symbol?)
       Integer)
 (@def y-or-n-p-insert-y () Nil)
 (@def y-or-n-p-insert-n () Nil)
 (@def y-or-n-p-insert-other () Nil)
 (@def use-dialog-box-p () Bool)
 (@def y-or-n-p (prompt: String) Boolean))


;;; ============================================================
;;; Atomic change groups

(et-declare
 (@check atomic-change-group ($body))
 (@check with-undo-amalgamate ($body))
 (@def prepare-change-group
       (&optional buffer: Buffer?)
       ListFresh<ConsFresh<Buffer~Any>>)
 (@def activate-change-group (handle: &List<&Cons<Buffer~Any>>) Nil)
 (@def accept-change-group (handle: &List<&Cons<Buffer~Any>>) Nil)
 (@def cancel-change-group (handle: &List<Cons<Buffer~Any>>) Nil))


;;; ============================================================
;;; Display-related functions

(et-declare
 (@def momentary-string-display
       (string: String pos: IntOrMarker
                &optional exit-char: Any message: String?)
       Nil))


;;; ============================================================
;;; Overlay operations

(et-declare
 (@def copy-overlay (overlay: Overlay) Overlay)
 (@def remove-overlays
       (&optional beg: IntOrMarker? end: IntOrMarker?
                  name: Symbol? val: Any)
       Nil))


;;; ============================================================
;;; Misc. useful functions

(et-declare
 (@def buffer-narrowed-p () Boolean)
 (@check with-restriction)
 (@check without-restriction)
 (@def find-tag-default-bounds () ConsFresh<Integer~Integer>?)
 (@def find-tag-default () String?)
 (@def find-tag-default-as-regexp () String?)
 (@def find-tag-default-as-symbol-regexp () String?)
 (@def play-sound)
 (@def shell-quote-argument (argument: String &optional posix: Bool) String)
 (@def string-to-list (string: String) ListFresh<Integer>)
 (@def string-to-vector (string: String) VectorFresh<Integer>)
 (@def string-or-null-p ([T] object: T) (is? T String|Nil))
 (@def list-of-strings-p ([T] object: T) (is? T List<String>))
 (@def booleanp ([T] object: T) (is? T Boolean))
 (@def special-form-p (object: Any) Boolean)
 (@def plistp ([T] object: T) (is-a? T PlistOf<Any~Any>))
 (@def macrop (object: Any) Boolean)
 (@def compiled-function-p
       ([T] object: T)
       (is-a? T Subr|ByteCodeFunction))
 (@def integer-or-null-p ([T] object: T) (is? T Integer|Nil))
 (@def field-at-pos (pos: IntOrMarker) Any)
 (@def sha1
       (object: StringOrBuffer<I> &optional start: [I] end: I binary: Bool)
       String)
 (@def function-get
       (function: Any prop: Symbol &optional autoload: Bool|@macro)
       Any))


;;; ============================================================
;;; Support for yanking and text properties

(et-declare
 (@def remove-yank-excluded-properties
       (start: IntOrMarker end: IntOrMarker)
       Nil)
 (@def insert-for-yank (string: String) Nil)
 (@def insert-for-yank-1 (string: String) Nil)
 (@def insert-buffer-substring-no-properties
       (buffer: Buffer|String &optional start: IntOrMarker? end: IntOrMarker?)
       Nil)
 (@def insert-buffer-substring-as-yank
       (buffer: Buffer|String &optional start: IntOrMarker? end: IntOrMarker?)
       Nil)
 (@def insert-into-buffer
       (buffer: Buffer|String &optional start: IntOrMarker? end: IntOrMarker?)
       Nil)
 (@def replace-string-in-region
       (string: String replacement: String
                &optional start: IntOrMarker? end: IntOrMarker?)
       Integer?)
 (@def replace-regexp-in-region
       (regexp: String replacement: String
                &optional start: IntOrMarker? end: IntOrMarker?)
       Integer?)
 (@def yank-handle-font-lock-face-property
       (face: Any start: IntOrMarker end: IntOrMarker)
       Nil)
 (@def yank-handle-category-property
       (category: Symbol start: IntOrMarker end: IntOrMarker)
       Nil))


;;; ============================================================
;;; Synchronous shell commands

(et-declare
 (@def start-process-shell-command
       (name: String buffer: Buffer|String? command: String)
       Process)
 (@def start-file-process-shell-command
       (name: String buffer: Buffer|String? command: String)
       Process)
 (@def call-process-shell-command
       (command: String &optional infile: String?
                 buffer: (or Buffer String Boolean Integer
                             (Tuple @file String)
                             (Tuple Buffer|String|Boolean String|Boolean))
                 display: Bool &rest args: &List<String>)
       Nil|Integer|String)
 (@def process-file-shell-command
       (command: String &optional infile: String?
                 buffer: (or Buffer String Boolean Integer
                             (Tuple @file String)
                             (Tuple Buffer|String|Boolean String|Boolean))
                 display: Bool &rest args: &List<String>)
       Nil|Integer|String)
 (@def call-shell-region
       (start: IntOrMarker end: IntOrMarker command: String
               &optional delete: Bool
               buffer: (or Buffer String Boolean Integer
                           (Tuple @file String)
                           (Tuple Buffer|String|Boolean String|Boolean)))
       Nil|Integer|String))


;;; ============================================================
;;; Lisp macros to do various things temporarily

(et-declare
 (@check track-mouse ($body))
 (@check with-current-buffer ($body Buffer|String))
 (@def generate-new-buffer
       (name: String &optional inhibit-buffer-hooks: Bool)
       Buffer)
 (@check with-selected-window ($body Window))
 (@check with-selected-frame ($body Frame))
 (@check save-window-excursion ($body))
 (@def internal-temp-output-buffer-show (buffer: Buffer|String) Nil)
 (@check with-output-to-temp-buffer ($body String))
 (@check with-temp-file ($body String))
 (@check with-temp-message ($body String?))
 (@check with-temp-buffer ($body))
 (@check with-silent-modifications ($body))
 (@check with-output-to-string)
 (@check with-local-quit)
 (@check while-no-input)
 (@check condition-case-unless-debug
         ($exp (cons 'condition-case (cdr (et-cur-expr)))))
 (@check with-demoted-errors)
 (@check combine-after-change-calls ($body))
 (@def combine-change-calls-1
       ([R] beg: IntOrMarker end: IntOrMarker body: (fn Nil R))
       R)
 (@check combine-change-calls ($body IntOrMarker IntOrMarker))
 (@check with-case-table ($body CharTable))
 (@check with-file-modes ($body Integer))
 (@check with-existing-directory ($body)))


;;; ============================================================
;;; Matching and match data

(et-declare
 (@check save-match-data ($body))
 (@def match-string (subexp: Integer &optional string: String) String?)
 (@def match-string-no-properties
       (subexp: Integer &optional string: String)
       String?)
 (@def match-substitute-replacement
       (replacement: String &optional fixedcase: Bool literal: Bool
                     string: String? subexp: Integer?)
       String?)
 (@def looking-back
       (regexp: String &optional limit: IntOrMarker? greedy: Bool)
       Boolean)
 (@def looking-at-p (regexp: String) Boolean)
 (@def string-match-p
       (regexp: String string: String &optional start: Integer?)
       Integer?)
 (@def subregexp-context-p
       (regexp: String pos: Integer &optional start: Integer?)
       Boolean))


;;; ============================================================
;;; Split-string

(et-declare
 (@def split-string
       (string: String &optional separators: String? omit-nulls: Bool
                trim: String?)
       ListFresh<String>)
 (@def string-split
       (string: String &optional separators: String? omit-nulls: Bool
                trim: String?)
       ListFresh<String>)
 (@def combine-and-quote-strings
       (strings: &List<String> &optional separator: String?)
       String)
 (@def split-string-and-unquote
       (string: String &optional separator: String?)
       ListFresh<String>))


;;; ============================================================
;;; Replacement in strings

(et-declare
 (@def subst-char-in-string
       (fromchar: Integer tochar: Integer string: String
                  &optional inplace: Bool)
       String)
 (@def string-replace (from-string: String to-string: String in-string: String) String)
 (@def replace-regexp-in-string
       (regexp: String rep: String|fn1<String~String> string: String
                &optional fixedcase: Bool literal: Bool subexp: Integer?
                start: Integer?)
       String)
 (@def string-equal-ignore-case
       (string1: String string2: String)
       Boolean)
 (@def string-prefix-p
       (prefix: String string: String &optional ignore-case: Bool)
       Boolean)
 (@def string-suffix-p
       (suffix: String string: String &optional ignore-case: Bool)
       Boolean)
 (@def bidi-string-mark-left-to-right (string: String) String)
 (@def string-greaterp
       (string1: Symbol|String string2: Symbol|String)
       Boolean))


;;; ============================================================
;;; Specifying things to do later

(et-declare
 (@def load-history-regexp (file: String) String)
 (@def load-history-filename-element)
 (@def eval-after-load (file: String|Symbol form: Any) Any)
 (@check with-eval-after-load)
 (@def do-after-load-evaluation (absolute-file: String) Nil)
 (@def display-delayed-warnings () Nil)
 (@def collapse-delayed-warnings () &List)
 (@def delay-warning
       (type: Symbol message: String &optional level: Symbol?
              buffer-name: String?)
       &List))


;;; ============================================================
;;; Invisibility specs

(et-declare
 (@def add-to-invisibility-spec (element: Any) &List)
 (@def remove-from-invisibility-spec (element: Any) &List))


;;; ============================================================
;;; Syntax tables

(et-declare
 (@check with-syntax-table ($body CharTable))
 (@def make-syntax-table (&optional oldtable: CharTable?) CharTable)
 (@def syntax-after (position: IntOrMarker) Integer|&Cons<Integer~Integer>?)
 (@def syntax-class (syntax: &Cons<Integer~Integer>?) Integer?)
 (@def forward-word-strictly (&optional arg: Integer?) Boolean)
 (@def backward-word-strictly (&optional arg: Integer?) Boolean)
 (@def forward-whitespace (arg: Integer) Integer?)
 (@def forward-symbol (arg: Integer) Integer?)
 (@def forward-same-syntax (&optional arg: Integer?) Nil))


;;; ============================================================
;;; Text clones

(et-declare
 (@def text-clone-create
       (start: IntOrMarker end: IntOrMarker &optional spreadp: Bool
               syntax: String?)
       &List<Overlay>))


;;; ============================================================
;;; Mail user agents

(et-declare
 (@def define-mail-user-agent
       (symbol: Symbol composefunc: AnyFn sendfunc: AnyFn
                &optional abortfunc: AnyFn? hookvar: Symbol?)
       Symbol))


;;; ============================================================
;;; Unsectioned functions

(et-declare
 (@def backtrace-frames)
 (@def backtrace-frame)
 (@def called-interactively-p (&optional kind: @interactive|@any?) Boolean)
 (@def interactive-p () Boolean)
 (@def internal-push-keymap)
 (@def internal-pop-keymap)
 (@def set-temporary-overlay-map
       (map: &Cons<@keymap~Any>|Symbol
             &optional keep-pred: True|fn? on-exit: fn?
             message: String|Bool timeout: Number?)
       fn)
 (@def set-transient-map
       (map: &Cons<@keymap~Any>|Symbol
             &optional keep-pred: True|fn? on-exit: fn?
             message: String|Bool timeout: Number?)
       fn))


;;; ============================================================
;;; Progress reporters

(et-declare
 (@def progress-reporter-update
       (reporter: Cons<Number~Vector<Any>>
                  &optional value: Number|String? suffix: String?)
       String?)
 (@def make-progress-reporter
       (message: String &optional min-value: Number? max-value: Number?
                 current-value: Number? min-change: Number? min-time: Number?)
       ConsFresh<Number~VectorFresh<Any>>)
 (@def progress-reporter-make
       (message: String &optional min-value: Number? max-value: Number?
                 current-value: Number? min-change: Number? min-time: Number?)
       ConsFresh<Number~VectorFresh<Any>>)
 (@def progress-reporter-force-update
       (reporter: Cons<Number~Vector<Any>>
                  &optional value: Number|String? new-message: String?
                  suffix: String?)
       String?)
 (@def progress-reporter-do-update
       (reporter: Cons<Number~Vector<Any>> value: Number|String?
                  &optional suffix: String?)
       String?)
 (@def progress-reporter-done (reporter: Cons) String)
 (@check dotimes-with-progress-reporter)
 (@check dolist-with-progress-reporter))


;;; ============================================================
;;; Comparing version strings

(et-declare
 (@def version-to-list (version: String) ListFresh<Integer>)
 (@def version-list-<
       (version1: &List<Integer> version2: &List<Integer>)
       Boolean)
 (@def version-list-=
       (version1: &List<Integer> version2: &List<Integer>)
       Boolean)
 (@def version-list-<=
       (version1: &List<Integer> version2: &List<Integer>)
       Boolean)
 (@def version-list-not-zero (version: &List<Integer>) Integer)
 (@def version< (version1: String version2: String) Boolean)
 (@def version<= (version1: String version2: String) Boolean)
 (@def version= (version1: String version2: String) Boolean))


;;; ============================================================
;;; Thread support

(et-declare
 (@check with-mutex ($body Mutex)))


;;; ============================================================
;;; Apropos

(et-declare
 (@def apropos-internal
       (regexp: String &optional predicate: fn1<Symbol>?)
       ListFresh<Symbol>))


;;; ============================================================
;;; Misc

(et-declare
 (@def register-definition-prefixes
       (file: String prefixes: &List<String>)
       Nil)
 (@def [flatten-tree flatten-list] (tree: [T]) (ListFresh (eval et:std-leaves-of T)))
 (@def string-trim-left (string: String &optional regexp: String?) String)
 (@def string-trim-right (string: String &optional regexp: String?) String)
 (@def string-trim
       (string: String &optional trim-left: String? trim-right: String?)
       String)
 (@def run-hook-query-error-with-timeout (hook: Symbol) Nil)
 (@def json-available-p () True)
 (@def ensure-list (object: [T]) (extends? T &List T ListFresh<T>))
 (@check with-delayed-message)
 (@def function-alias-p
       (function: Any &optional noerror: Bool)
       ListFresh<Symbol>)
 (@def readablep (object: Any) String?)
 (@def delete-line () Nil)
 (@def ensure-empty-lines (&optional lines: Integer?) Nil)
 (@def string-lines
       (string: String &optional omit-nulls: Bool keep-newlines: Bool)
       ListFresh<String>)
 (@def buffer-match-p)
 (@def match-buffers
       (condition: Any &optional buffers: &List<Buffer>?
                   &rest args: &List)
       ListFresh<Buffer>)
 (@check handler-bind)
 (@check with-memoization))

(defvar et:std--leaves-of-stack nil)

(defun et:std-leaves-of (type)
  "Return the union of the non-nil leaves in TYPE."
  (et-stop-recursion et:std--leaves-of-stack type (et-never)
    (apply
     #'et-union
     (cl-loop
      for case in (et:type->cases type)
      for value = (et:type-case->value case)
      collect
      (pcase value
        ((cl-struct et:type-alias)
         (et:std-leaves-of (et:type-alias-expand value)))
        ((cl-struct et:type-dt (name 'Literal) (args '(nil)))
         (et-never))
        ((cl-struct et:type-dt (name 'ConsFresh) (args `(,car ,cdr)))
         (et-union (et:std-leaves-of car) (et:std-leaves-of cdr)))
        ((cl-struct et:type-dt (name 'ConsRW)
                    (args `(,car ,_car-write ,cdr ,_cdr-write)))
         (et-union (et:std-leaves-of car) (et:std-leaves-of cdr)))
        ((cl-struct et:type-dt (name 'PlistRW) args)
         (apply #'et-union
                (cl-loop for (key read _write) on args by #'cdddr
                         nconc (list (et:std-leaves-of key)
                                     (et:std-leaves-of read)))))
        ((cl-struct et:type-dt (name (or 'Function 'PolyFunction)))
         (et-any))
        (_ (et-type case)))))))


;;; ============================================================
