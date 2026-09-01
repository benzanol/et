;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Macro expansion core

(et-declare
 (@variable byte-compile-form-stack List<Sexp>)
 (@variable macroexpand-all-environment Alist<Symbol~Any>)
 (@variable macroexp-inhibit-compiler-macros Boolean)
 (@def macroexp-compiling-p () &List<Cons<Symbol~Any>>)
 (@def macroexp-file-name () String?)
 (@def macroexp-warn-and-return
       (msg: String? form: Sexp
        &optional category: Symbol|List<Symbol>? compile-only: Bool arg: Sexp?)
       Sexp)
 (@def macroexpand-1 (form: Sexp &optional environment: Alist<Symbol~Any>) Sexp)
 (@def macroexp-macroexpand (form: Sexp env: Alist<Symbol~Any>) Sexp)
 (@def macroexpand-all (form: Sexp &optional environment: Alist<Symbol~Any>) Sexp))

;;; ============================================================
;;; Handy functions to use in macros

(et-declare
 (@def macroexp-parse-body (body: &List<Sexp>) Cons<List<Sexp>~List<Sexp>>)
 (@def macroexp-progn (exps: &List<Sexp>) Sexp)
 (@def macroexp-unprogn (exp: Sexp) Cons<Sexp~List<Sexp>>)
 (@def macroexp-let* (bindings: &List<Sexp> exp: Sexp) Sexp)
 (@def macroexp-if (test: Sexp then: Sexp else: Sexp) Sexp)
 ;; This macro binds SYM as a fresh lexical variable while checking BODY,
 ;; with SYM's type depending on whether EXP is const per TEST. The $body
 ;; and $fn shortcuts cannot introduce a lexical binding visible while
 ;; checking the remaining operands.
 (@check macroexp-let2 ($todo))
 ;; Same blocker as `macroexp-let2': this macro introduces a lexical
 ;; binding per entry in BINDINGS, visible while checking later bindings
 ;; and the body. Neither $body nor $fn can introduce such bindings.
 (@check macroexp-let2* ($todo))
 (@def macroexp-small-p (exp: Sexp) Boolean)
 (@def macroexp-const-p (exp: Sexp) Bool)
 (@def macroexp-copyable-p (exp: Sexp) Bool)
 (@def macroexp-quote (v: [T]) (or T (Tuple @quote T))))

;;; ============================================================
;;; Load-time macro-expansion

(et-declare
 (@def internal-macroexpand-for-load (form: Sexp full-p: Bool) Sexp))

;;; ============================================================
