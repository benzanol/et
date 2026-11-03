;; -*- lexical-binding: t; -*-

(et-declare
 (@def case-table-p (object: Any) Boolean)
 (@def current-case-table () CharTable)
 (@def standard-case-table () CharTable)
 (@def set-case-table (table: CharTable) CharTable)
 (@def set-standard-case-table (table: CharTable) CharTable))
