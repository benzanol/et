;; -*- lexical-binding: t; -*-

(et-declare
 (@check tabulated-list-get-id ($fn IntOrMarker? Any))
 (@check tabulated-list-get-entry ($fn IntOrMarker? &Vector<String|&Cons<Any_Any>>?))
 (@def tabulated-list-put-tag (tag: String &optional advance: Bool) Integer|Nil)
 (@def tabulated-list-clear-all-tags () Nil)
 (@def tabulated-list-make-glyphless-char-display-table () CharTable)
 (@def tabulated-list-init-header () Any)
 (@def tabulated-list-print-fake-header () Any)
 (@check tabulated-list-header-overlay-p ($fn IntOrMarker? Overlay|Nil))
 (@def tabulated-list-revert (&rest _ignored: &List) Integer)
 (@def tabulated-list-print (&optional remember-pos: Bool update: Bool) Integer)
 (@def tabulated-list-print-entries
       (entries: &List<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>>
        sorter: fn2<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>_Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>_Boolean>|Nil
        update: Bool
        entry-id: Any)
       Integer|Nil)
 (@def tabulated-list-print-entry (id: Any cols: &Vector<String|&Cons<Any_Any>>) Boolean)
 (@def tabulated-list-print-col (n: Integer col-desc: String|&Cons<Any_Any> x: Integer) Integer)
 (@def tabulated-list-delete-entry () Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>|Nil)
 (@def tabulated-list-set-col
       (col: Integer|String desc: String|&Cons<Any_Any> &optional change-entry-data: Bool)
       Integer|Nil)
 (@def tabulated-list-col-sort (&optional e: Any) Integer|Nil)
 (@def tabulated-list-sort (&optional n: Integer?) Integer|Nil)
 (@def tabulated-list-widen-current-column (&optional n: Integer?) Any)
 (@def tabulated-list-narrow-current-column (&optional n: Integer?) Any)
 (@def tabulated-list-next-column (&optional arg: Integer?) Nil)
 (@def tabulated-list-previous-column (&optional arg: Integer?) Nil))

;;; ============================================================
;;; The mode definition

(et-declare
 (@def tabulated-list-mode () Any))

;;; ============================================================
;;; Tabulated list groups

(et-declare
 ;; SORT-FUNCTION operates on the intermediate group tree, a
 ;; self-referential alist mapping strings to either a nested subtree or a
 ;; vector of entries. The type language cannot yet express recursive,
 ;; self-referential structural types without defining a new alias, which
 ;; authoring forbids.
 (@def tabulated-list-groups
       (entries: &List<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>>
        metadata: (Plist
                   :path-function fn1<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>_&List<&List<String>>>
                   :sort-function fn2<Todo_Integer_Todo>|Nil))
       List<Cons<String_List<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>>>>)
 ;; The result tree is a self-referential alist mapping strings to either a
 ;; nested subtree or a vector of entries. The type language cannot yet
 ;; express recursive, self-referential structural types without defining a
 ;; new alias, which authoring forbids.
 (@def tabulated-list-groups-categorize
       (entries: &List<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>>
        path-function: fn1<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>_&List<&List<String>>>)
       Todo)
 ;; TREE and SORT-FUNCTION operate on the self-referential group tree
 ;; structure. The type language cannot yet express recursive,
 ;; self-referential structural types without defining a new alias, which
 ;; authoring forbids.
 (@def tabulated-list-groups-sort
       (tree: Todo sort-function: fn2<Todo_Integer_Todo> &optional level: Integer?)
       Todo)
 ;; TREE is the self-referential group tree structure. The type language
 ;; cannot yet express recursive, self-referential structural types without
 ;; defining a new alias, which authoring forbids.
 (@def tabulated-list-groups-flatten
       (tree: Todo)
       List<Cons<String_List<Cons<Any_Cons<&Vector<String|&Cons<Any_Any>>_Nil>>>>>))

;;; ============================================================
