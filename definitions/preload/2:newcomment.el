;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Aliases and variables

(et-declare
 (@def indent-for-comment (&optional continue: Bool) Any)
 (@def set-comment-column (arg: Any) Any)
 (@def kill-comment (arg: Any) Nil)
 (@def indent-new-comment-line (&optional soft: Bool) Any)
 (@variable comment-use-syntax Boolean|@undecided)
 (@variable comment-fill-column Integer?)
 (@variable comment-column Integer)
 (@variable comment-start String?)
 (@variable comment-start-skip String?)
 (@variable comment-end-skip String?)
 (@variable comment-end String)
 (@variable comment-indent-function (fn Nil Integer|Cons<Integer~Integer>?))
 (@variable comment-insert-comment-function fn?)
 (@variable comment-combine-change-calls Boolean)
 (@variable comment-region-function (fn (Args IntOrMarker IntOrMarker Any?) Any))
 (@variable uncomment-region-function (fn (Args IntOrMarker IntOrMarker Any?) Any))
 (@variable block-comment-start String?)
 (@variable block-comment-end String?)
 (@variable comment-quote-nested Boolean)
 (@variable comment-quote-nested-function (fn (Args String String Bool)))
 (@variable comment-continue String?)
 (@variable comment-add Integer)
 (@variable comment-styles List<Tuple<Symbol~Boolean~Boolean~Boolean~Boolean|@multi-char~String>>)
 (@variable comment-style @plain|@indent-or-triple|@indent|@aligned|@box|@extra-line|@multi-line|@box-multi)
 (@variable comment-padding String|Integer?)
 (@variable comment-inline-offset Integer)
 (@variable comment-multi-line Boolean)
 (@variable comment-empty-lines Boolean|@eol))

;;; ============================================================
;;; Helpers

(et-declare
 (@def comment-string-strip (str: String beforep: Bool afterp: Bool) String)
 (@def comment-string-reverse (s: String) String)
 (@def comment-normalize-vars (&optional noerror: Bool) Any)
 (@def comment-quote-re (str: String unp: Bool) String)
 (@def comment-quote-nested (cs: String ce: String unp: Bool) Any)
 (@def comment-quote-nested-default (cs: String ce: String unp: Bool) Nil))

;;; ============================================================
;;; Navigation

(et-declare
 (@variable comment-use-global-state Boolean)
 (@def comment-search-forward (limit: IntOrMarker? &optional noerror: Bool) Integer?)
 (@def comment-search-backward (&optional limit: IntOrMarker? noerror: Bool) Integer?)
 (@def comment-beginning () Integer?)
 (@def comment-forward (&optional n: Integer?) Boolean)
 (@def comment-enter-backward () Any))

;;; ============================================================
;;; Commands

(et-declare
 (@def comment-indent-default () Integer?)
 (@def comment-choose-indent (&optional indent: Integer|Cons<Integer~Integer>?) Integer)
 (@def comment-indent (&optional continue: Bool) Any)
 (@def comment-set-column (arg: Any) Any)
 (@def comment-kill (arg: Any) Nil)
 (@def comment-padright (str: String? &optional n: Integer|@re?) String?)
 (@def comment-padleft (str: String? &optional n: Integer|@re?) String?)
 (@def uncomment-region (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def uncomment-region-default-1 (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def uncomment-region-default (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def comment-make-bol-ws (len: Integer) String)
 (@def comment-make-extra-lines (cs: String ce: String ccs: String cce: String min-indent: Integer max-indent: Integer &optional block: Bool) Cons<String~String>)
 (@check comment-with-narrowing ($body IntOrMarker IntOrMarker))
 (@def comment-add (arg: Any) Integer)
 (@def comment-region-internal (beg: IntOrMarker end: IntOrMarker cs: String ce: String? &optional ccs: String? cce: String? block: Bool lines: Bool indent: Bool) Nil)
 (@def comment-region (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def comment-region-default-1 (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def comment-region-default (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def comment-box (beg: IntOrMarker end: IntOrMarker &optional arg: Integer) Any)
 (@def comment-only-p (beg: IntOrMarker end: IntOrMarker) Boolean)
 (@def comment-or-uncomment-region (beg: IntOrMarker end: IntOrMarker &optional arg: Any) Any)
 (@def comment-dwim (arg: Any) Any)
 (@variable comment-auto-fill-only-comments Boolean)
 (@def comment-valid-prefix-p (prefix: String compos: Integer?) Boolean)
 (@def comment-indent-new-line (&optional soft: Bool) Any)
 (@def comment-line (n: Integer) Nil|@comment-line-backward))

;;; ============================================================
