;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Menu bar lookup

(et-declare
 (@def menu-bar-menu-at-x-y (x: Integer y: Integer &optional frame: Frame?) Symbol?))

;;; ============================================================
;;; Popup menus

(et-declare
 ;; POSITION may be a mouse-button or touch-screen event, an object shape
 ;; the type system does not yet model. The return value is either a list
 ;; of chosen events or the arbitrary VALUE of the chosen menu item,
 ;; depending on whether MENU is a keymap or a pane list; Any covers both.
 (@def x-popup-menu
       (position: (or Boolean (Tuple (Tuple Integer Integer) Window|Frame) Todo)
        menu: (or Cons<@keymap~Any> List<Cons<@keymap~Any>>
                  Cons<String~List<Cons<String~List<Cons<String~Any>|String>>>>))
       Any))

;;; ============================================================
;;; Popup dialogs

(et-declare
 ;; POSITION may be a mouse-button event or an EVENT_START-style position
 ;; list, an object shape the type system does not yet model. The return
 ;; value is the arbitrary VALUE of the chosen item.
 (@def x-popup-dialog
       (position: (or Boolean Window Frame (Tuple (Tuple Integer Integer) Window|Frame) Todo)
        contents: (Tuple* Any List<Cons<String~Any>|String?>)
        &optional header: Bool)
       Any))

;;; ============================================================
