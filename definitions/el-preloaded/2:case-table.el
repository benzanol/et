;; -*- lexical-binding: t; -*-

(et-declare
 (@def describe-buffer-case-table () Any)
 (@def case-table-get-table (case-table: CharTable table: @down|@up|@eqv|@canon) CharTable|Nil)
 (@def get-upcase-table (case-table: CharTable) CharTable|Nil)
 (@def copy-case-table (case-table: CharTable) CharTable)
 (@def set-case-syntax-delims (l: Integer r: Integer table: CharTable) Nil)
 (@def set-case-syntax-pair (uc: Integer lc: Integer table: CharTable) Nil)
 (@def set-upcase-syntax (uc: Integer lc: Integer table: CharTable) Nil)
 (@def set-downcase-syntax (uc: Integer lc: Integer table: CharTable) Nil)
 (@def set-case-syntax (c: Integer syntax: String table: CharTable) Nil))
