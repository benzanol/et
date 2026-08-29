;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Font Lock mode

(et-declare
 (@def font-lock-specified-p (mode: Bool) Any)
 (@def font-lock-initial-fontify () Any)
 (@def font-lock-mode-internal (arg: Bool) Any)
 (@def font-lock-add-keywords (mode: Symbol? keywords: &List &optional how: Symbol?) Any)
 (@def font-lock-update-removed-keyword-alist (mode: Symbol? keywords: &List how: Symbol?) Any)
 (@def font-lock-remove-keywords (mode: Symbol? keywords: &List) Any))

;;; ============================================================
;;; Font Lock Support mode

(et-declare
 (@def font-lock-turn-on-thing-lock () Any)
 (@def font-lock-turn-off-thing-lock () Any))

;;; ============================================================
;;; Fontification functions

(et-declare
 (@def font-lock-fontify-buffer (&optional interactively: Bool) Any)
 (@def font-lock-unfontify-buffer () Any)
 (@def font-lock-fontify-region (beg: IntOrMarker end: IntOrMarker &optional loudly: Bool) Any)
 (@def font-lock-unfontify-region (beg: IntOrMarker end: IntOrMarker) Any)
 (@def font-lock-flush (&optional beg: IntOrMarker? end: IntOrMarker?) Any)
 (@def font-lock-debug-fontify () Any)
 (@def font-lock-ensure (&optional beg: IntOrMarker? end: IntOrMarker?) Any)
 (@def font-lock-update (&optional arg: Bool) Any)
 (@def font-lock-default-fontify-buffer () Any)
 (@def font-lock-default-unfontify-buffer () Nil)
 (@def font-lock-extend-region-multiline () Boolean)
 (@def font-lock-extend-region-wholelines () Boolean)
 (@def font-lock-default-fontify-region (beg: IntOrMarker end: IntOrMarker loudly: Bool) Cons<@jit-lock-bounds~Cons<IntOrMarker~IntOrMarker>>)
 (@def font-lock-default-unfontify-region (beg: IntOrMarker end: IntOrMarker) Any)
 (@def font-lock-after-change-function (beg: IntOrMarker end: IntOrMarker &optional old-len: Integer?) Any)
 (@def font-lock-extend-jit-lock-region-after-change (beg: IntOrMarker end: IntOrMarker old-len: Integer) Integer)
 (@def font-lock-fontify-block (&optional arg: Integer?) Any))

;;; ============================================================
;;; Additional text property functions

(et-declare
 (@def font-lock-prepend-text-property ([(<= Idx IntOrMarker)] start: Idx end: Idx prop: Symbol value: Any &optional object: StringOrBuffer<Idx>?) Nil)
 (@def font-lock-append-text-property ([(<= Idx IntOrMarker)] start: Idx end: Idx prop: Symbol value: Any &optional object: StringOrBuffer<Idx>?) Nil)
 (@def font-lock-fillin-text-property ([(<= Idx IntOrMarker)] start: Idx end: Idx prop: Symbol value: Any &optional object: StringOrBuffer<Idx>?) Nil))

;;; ============================================================
;;; Syntactic regexp fontification functions

(et-declare
 (@def font-lock-apply-syntactic-highlight (highlight: &List) Any)
 (@def font-lock-fontify-syntactic-anchored-keywords (keywords: &List limit: IntOrMarker) Any)
 (@def font-lock-fontify-syntactic-keywords-region (start: IntOrMarker end: IntOrMarker) Nil))

;;; ============================================================
;;; Syntactic fontification functions

(et-declare
 (@def font-lock-fontify-syntactically-region (beg: IntOrMarker end: IntOrMarker &optional loudly: Bool) Any)
 (@def font-lock-default-fontify-syntactically (start: IntOrMarker end: IntOrMarker &optional loudly: Bool) Nil))

;;; ============================================================
;;; Keyword regexp fontification functions

(et-declare
 (@def font-lock-apply-highlight (highlight: &List) Any)
 (@def font-lock-fontify-anchored-keywords (keywords: &List limit: IntOrMarker) Any)
 (@def font-lock-fontify-keywords-region (start: IntOrMarker end: IntOrMarker &optional loudly: Bool) Marker))

;;; ============================================================
;;; Various functions

(et-declare
 (@def font-lock-compile-keywords (keywords: &List &optional syntactic-keywords: Bool) List<Any>)
 (@def font-lock-compile-keyword (keyword: Any) List<Any>)
 (@def font-lock-eval-keywords (keywords: Any) List<Any>)
 (@def font-lock-value-in-major-mode (values: Any) Any)
 (@def font-lock-choose-keywords (keywords: Any level: Integer|True?) Any)
 (@def font-lock-refresh-defaults () Any)
 (@def font-lock-set-defaults () Any))

;;; ============================================================
;;; Various regexp information shared by several modes

(et-declare
 (@def font-lock-match-c-style-declaration-item-and-skip-to-next (limit: IntOrMarker) Integer|Boolean)
 (@def font-lock-after-fontify-buffer (&rest args: &List) Nil)
 (@def font-lock-after-unfontify-buffer (&rest args: &List) Nil))

;;; ============================================================
