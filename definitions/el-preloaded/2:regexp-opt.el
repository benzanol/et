;; -*- lexical-binding: t; -*-

(et-declare
 (@def regexp-opt (strings: &List<String> &optional paren: String|@words|@symbols|Bool) String)
 (@def regexp-opt-depth (regexp: String) Integer)
 (@def regexp-opt-group (strings: &List<String> &optional paren: String|Bool lax: Bool) String)
 (@def regexp-opt-charset (chars: &List<Integer>) String))
