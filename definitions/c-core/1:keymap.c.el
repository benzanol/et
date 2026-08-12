;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Keymap construction

(et-declare
;; AUTHORING STUB: not yet classified.
(@def make-keymap (&optional string: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-sparse-keymap (&optional string: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def keymapp (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def keymap-prompt (map: Todo) Todo))

;;; ============================================================
;;; Keymap inheritance

(et-declare
;; AUTHORING STUB: not yet classified.
(@def keymap-parent (keymap: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-keymap-parent (keymap: Todo parent: Todo) Todo))

;;; ============================================================
;;; Keymap traversal and copying

(et-declare
;; AUTHORING STUB: not yet classified.
(@def map-keymap-internal (function: Todo keymap: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def map-keymap (function: Todo keymap: Todo &optional sort_first: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def copy-keymap (keymap: Todo) Todo))

;;; ============================================================
;;; Key definitions and lookup

(et-declare
;; AUTHORING STUB: not yet classified.
(@def define-key (keymap: Todo key: Todo def: Todo &optional remove: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def command-remapping (command: Todo &optional position: Todo keymaps: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def lookup-key (keymap: Todo key: Todo &optional accept_default: Todo) Todo))

;;; ============================================================
;;; Current keymaps and bindings

(et-declare
;; AUTHORING STUB: not yet classified.
(@def current-active-maps (&optional olp: Todo position: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def key-binding (key: Todo &optional accept_default: Todo no_remap: Todo position: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def minor-mode-key-binding (key: Todo &optional accept_default: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def use-global-map (keymap: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def use-local-map (keymap: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def current-local-map () Todo)
;; AUTHORING STUB: not yet classified.
(@def current-global-map () Todo)
;; AUTHORING STUB: not yet classified.
(@def current-minor-mode-maps () Todo))

;;; ============================================================
;;; Keymap discovery and descriptions

(et-declare
;; AUTHORING STUB: not yet classified.
(@def accessible-keymaps (keymap: Todo &optional prefix: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def key-description (keys: Todo &optional prefix: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def single-key-description (key: Todo &optional no_angles: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def text-char-description (character: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def where-is-internal (definition: Todo &optional keymap: Todo firstonly: Todo noindirect: Todo no_remap: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def describe-buffer-bindings (buffer: Todo &optional prefix: Todo menus: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def describe-vector (vector: Todo &optional describer: Todo) Todo))

;;; ============================================================
