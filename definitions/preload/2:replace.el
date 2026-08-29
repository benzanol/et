;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Configuration variables

(et-declare
 (@variable case-replace Boolean)
 (@variable replace-char-fold Boolean)
 (@variable replace-lax-whitespace Boolean)
 (@variable replace-regexp-lax-whitespace Boolean)
 (@variable query-replace-history List<String>)
 (@variable query-replace-defaults List<Cons<String~String>>)
 (@variable query-replace-from-to-separator String?)
 (@variable query-replace-from-history-variable Symbol)
 (@variable query-replace-to-history-variable Symbol)
 (@variable query-replace-skip-read-only Boolean)
 (@variable query-replace-show-replacement Boolean)
 (@variable query-replace-highlight Boolean)
 (@variable query-replace-highlight-submatches Boolean)
 (@variable query-replace-lazy-highlight Boolean)
 (@variable replace-count Integer))

;;; ============================================================
;;; Query-replace argument reading

(et-declare
 (@def query-replace-descr (string: String) String)
 (@variable query-replace-read-from-default (or Nil (fn Nil String)))
 (@variable query-replace-read-from-regexp-default (or Nil (fn Nil String)))
 (@def query-replace-read-from-suggestions () List<String>)
 (@def query-replace-read-from (prompt: String regexp-flag: Bool) String|Cons<String~String|Cons<AnyFn~Any>>)
 (@def query-replace-compile-replacement (to: String regexp-flag: Bool) String|Cons<AnyFn~Any>)
 (@def query-replace-read-to (from: String prompt: String regexp-flag: Bool) String|Cons<AnyFn~Any>)
 (@def query-replace-read-args (prompt: String regexp-flag: Bool &optional noerror: Bool) Tuple<String~String|Cons<AnyFn~Any>~Any~Boolean>))

;;; ============================================================
;;; Query-replace commands

(et-declare
 (@def query-replace (from-string: String to-string: String|Cons<AnyFn~Any> &optional delimited: Any start: IntOrMarker? end: IntOrMarker? backward: Boolean region-noncontiguous-p: Any) Any)
 (@def query-replace-regexp (regexp: String to-string: String|Cons<AnyFn~Any> &optional delimited: Any start: IntOrMarker? end: IntOrMarker? backward: Boolean region-noncontiguous-p: Any) Any)
 (@def map-query-replace-regexp (regexp: String to-strings: String|List<String> &optional n: Integer? start: IntOrMarker? end: IntOrMarker? region-noncontiguous-p: Any) Any)
 (@def replace-string (from-string: String to-string: String|Cons<AnyFn~Any> &optional delimited: Any start: IntOrMarker? end: IntOrMarker? backward: Boolean region-noncontiguous-p: Any) Any)
 (@def replace-regexp (regexp: String to-string: String|Cons<AnyFn~Any> &optional delimited: Any start: IntOrMarker? end: IntOrMarker? backward: Boolean region-noncontiguous-p: Any) Any))

;;; ============================================================
;;; Reading regexps

(et-declare
 (@variable regexp-history List<String>)
 (@variable occur-highlight-overlays List<Overlay>)
 (@variable occur-collect-regexp-history List<String>)
 (@variable read-regexp-defaults-function Nil|@regexp-history-last|(fn Nil String|List<String>?))
 (@def read-regexp-suggestions () List<String?>)
 (@variable read-regexp-map Cons<@keymap~Any>)
 (@def read-regexp-toggle-case-fold () Any)
 (@def read-regexp (prompt: String &optional defaults: Nil|String|List<String>|Symbol history: Symbol?) String)
 (@def read-regexp-case-fold-search (regexp: String) Boolean))

;;; ============================================================
;;; Keep, flush, and count matching lines

(et-declare
 (@def delete-non-matching-lines (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Nil)
 (@def delete-matching-lines (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Integer)
 (@def count-matches (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Integer)
 (@def keep-lines-read-args (prompt: String) Tuple<String~Nil~Nil~True>)
 (@def keep-lines (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Nil)
 (@def flush-lines (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Integer)
 (@def kill-matching-lines (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Integer)
 (@def copy-matching-lines (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Integer)
 (@def how-many (regexp: String &optional rstart: IntOrMarker? rend: IntOrMarker? interactive: Bool) Integer))

;;; ============================================================
;;; Occur mode

(et-declare
 (@variable occur-menu-map Cons<@keymap~Any>)
 (@variable occur-mode-map Cons<@keymap~Any>)
 (@variable occur-revert-arguments Tuple<String~Integer?~List<Buffer>>)
 (@variable occur-mode-hook List<AnyFn>)
 (@variable occur-hook List<AnyFn>)
 (@variable occur-mode-find-occurrence-hook List<AnyFn>)
 (@def occur-mode () Any)
 (@variable occur-edit-mode-map Cons<@keymap~Any>)
 (@def occur-edit-mode () Any)
 (@variable occur-edit-mode-hook List<AnyFn>)
 (@def occur-cease-edit () Any)
 (@def occur-after-change-function (beg: Integer end: Integer length: Integer) Any)
 (@def occur-revert-function (_ignore1: Any _ignore2: Any) Any)
 (@def occur-mode-find-occurrence () Marker)
 (@def occur-mode-mouse-goto (&optional event: Any) Any)
 (@def occur-mode-goto-occurrence (&optional event: Any) Any)
 (@def occur-mode-goto-occurrence-other-window () Any)
 (@def occur-goto-locus-delete-o () Any)
 (@def occur-mode-display-occurrence () Any)
 (@def occur-find-match (n: Integer? search: (fn (Args IntOrMarker Symbol) IntOrMarker?) message: String) Nil)
 (@def occur-next (&optional n: Integer?) Nil)
 (@def occur-prev (&optional n: Integer?) Nil)
 (@def occur-next-error (&optional argp: Integer reset: Bool) Any))

;;; ============================================================
;;; Occur customization and entry points

(et-declare
 (@variable list-matching-lines-default-context-lines Integer)
 (@def list-matching-lines (regexp: String &optional nlines: Integer|String? region: &List<&Cons<Integer~Integer>>?) Any)
 (@variable list-matching-lines-face Symbol?)
 (@variable list-matching-lines-buffer-name-face Symbol?)
 (@variable list-matching-lines-current-line-face Symbol)
 (@variable list-matching-lines-jump-to-current-line Boolean)
 (@variable list-matching-lines-prefix-face Symbol)
 (@variable occur-excluded-properties True|List<Symbol>)
 (@def occur-read-primary-args () Tuple<String~String|Integer?>)
 (@def occur-rename-buffer (&optional unique-p: Bool interactive-p: Bool) String)
 (@def occur (regexp: String &optional nlines: Integer|String? region: &List<&Cons<Integer~Integer>>?) Any)
 (@def multi-occur (bufs: &List<Buffer> regexp: String &optional nlines: Integer?) Any)
 (@def multi-occur-in-matching-buffers (bufregexp: String regexp: String &optional allbufs: Bool) Any)
 (@def occur-regexp-descr (regexp: String) String)
 (@def occur-1 (regexp: String nlines: Integer|String? bufs: &List<Buffer|Overlay> &optional buf-name: String?) Any))

;;; ============================================================
;;; Occur engine

(et-declare
 (@def occur-engine (regexp: String buffers: &List<Buffer|Overlay> out-buf: Buffer nlines: Integer case-fold: Bool title-face: Symbol prefix-face: Symbol? match-face: Symbol? keep-props: Bool) Integer)
 (@def occur-engine-line (beg: IntOrMarker end: IntOrMarker &optional keep-props: Bool) String)
 (@def occur-engine-add-prefix (lines: &List<String> &optional prefix-face: Symbol?) List<String>)
 (@def occur-accumulate-lines (count: Integer &optional keep-props: Bool pt: IntOrMarker?) List<String>)
 (@def occur-context-lines (out-line: String nlines: Integer keep-props: Bool begpt: IntOrMarker endpt: IntOrMarker curr-line: Integer prev-line: Integer? prev-after-lines: List<String>? &optional prefix-face: Symbol? orig-line: Integer? multi-occur-p: Bool) Tuple<String~List<String>?>)
 (@def occur-word-at-mouse (event: Any) Any)
 (@def occur-symbol-at-mouse (event: Any) Any)
 (@def occur-context-menu (menu: Cons<@keymap~Any> click: Any) Cons<@keymap~Any>))

;;; ============================================================
;;; Replace internals

(et-declare
 (@variable query-replace-help String)
 (@variable query-replace-map Cons<@keymap~Any>)
 (@variable multi-query-replace-map Cons<@keymap~Any>)
 (@def replace-match-string-symbols (n: List<Any>) Nil)
 (@def replace-eval-replacement (expression: Any count: Integer) String)
 (@def replace-quote (replacement: Any) String)
 (@def replace-loop-through-replacements (data: Vector<Any> count: Integer) Any)
 (@def replace-match-data (integers: Bool reuse: List<Integer|Marker?>? &optional new: List<Integer|Marker?>?) List<Integer|Marker?>)
 (@def replace-match-maybe-edit (newtext: String fixedcase: Bool literal: Bool noedit: Bool match-data: &List<Integer|Marker?> &optional backward: Bool) Boolean)
 (@variable replace-update-post-hook List<AnyFn>)
 (@variable replace-search-function (or Nil (fn (Args String IntOrMarker? Any) Integer?)))
 (@variable replace-re-search-function (or Nil (fn (Args String IntOrMarker? Any) Integer?)))
 (@variable replace-regexp-function Boolean|(fn (Args String Bool?) String))
 (@def replace-search (search-string: String limit: IntOrMarker? regexp-flag: Bool delimited-flag: Any case-fold: Bool &optional backward: Bool) Integer?)
 (@variable replace-overlay Overlay?)
 (@variable replace-submatches-overlays List<Overlay>)
 (@def replace-highlight (match-beg: IntOrMarker match-end: IntOrMarker range-beg: IntOrMarker range-end: IntOrMarker search-string: String regexp-flag: Bool delimited-flag: Any case-fold: Bool &optional backward: Bool) Any)
 (@def replace-dehighlight () Any))

;;; ============================================================
;;; Perform replace

(et-declare
 (@def perform-replace
       (from-string: String replacements: String|List<String>|Cons<AnyFn~Any>
        query-flag: Bool regexp-flag: Bool delimited-flag: Any
        &optional repeat-count: Integer? map: Cons<@keymap~Any>? start: IntOrMarker?
        end: IntOrMarker? backward: Bool region-noncontiguous-p: Any)
       List<Any>|Boolean))

;;; ============================================================
