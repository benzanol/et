;; -*- lexical-binding: t; -*-

(et-declare
 ;; `define-widget-keywords' is an obsolete macro that discards its
 ;; `&rest' argument forms without ever evaluating them, always
 ;; expanding to `nil'. Neither `$body' (which evaluates trailing
 ;; operands as a body) nor `$fn' (which checks operands as evaluated
 ;; expressions) can model a rest argument list that is never
 ;; evaluated. A future checker needs a way to accept and discard
 ;; unevaluated operand forms.
 (@check define-widget-keywords ($todo))
 (@def define-widget (name: [(<= T Symbol)] class: Symbol doc: String? &rest args: &List) T))
