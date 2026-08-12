;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Window predicates and relationships

(et-declare
;; AUTHORING STUB: not yet classified.
(@def windowp (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-valid-p (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-live-p (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-frame (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-root-window (&optional frame_or_window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-window (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-minibuffer-p (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-first-window (&optional frame_or_window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-selected-window (&optional frame_or_window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def frame-old-selected-window (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-frame-selected-window (frame: Todo window: Todo &optional norecord: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def selected-window () Todo)
;; AUTHORING STUB: not yet classified.
(@def old-selected-window () Todo)
;; AUTHORING STUB: not yet classified.
(@def select-window (window: Todo &optional norecord: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-buffer (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-old-buffer (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-parent (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-top-child (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-left-child (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-next-sibling (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-prev-sibling (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-combination-limit (window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-combination-limit (window: Todo limit: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-use-time (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-bump-use-time (&optional window: Todo) Todo))

;;; ============================================================
;;; Window dimensions

(et-declare
;; AUTHORING STUB: not yet classified.
(@def window-pixel-width (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-pixel-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-old-pixel-width (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-old-pixel-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-total-height (&optional window: Todo round: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-total-width (&optional window: Todo round: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-new-total (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-normal-size (&optional window: Todo horizontal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-new-normal (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-new-pixel (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-pixel-left (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-pixel-top (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-left-column (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-top-line (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-body-width (&optional window: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-body-height (&optional window: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-old-body-pixel-width (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-old-body-pixel-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-mode-line-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-header-line-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-tab-line-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-right-divider-width (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-bottom-divider-width (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-scroll-bar-width (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-scroll-bar-height (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-hscroll (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-hscroll (window: Todo ncol: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def coordinates-in-window-p (coordinates: Todo window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-at (x: Todo y: Todo &optional frame: Todo) Todo))

;;; ============================================================
;;; Window positions and display

(et-declare
;; AUTHORING STUB: not yet classified.
(@def window-point (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-old-point (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-start (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-end (&optional window: Todo update: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-point (window: Todo pos: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-start (window: Todo pos: Todo &optional noforce: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def pos-visible-in-window-p (&optional pos: Todo window: Todo partially: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-line-height (&optional line: Todo window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-lines-pixel-dimensions (&optional window: Todo first: Todo last: Todo body: Todo inverse: Todo left: Todo) Todo))

;;; ============================================================
;;; Window properties and traversal

(et-declare
;; AUTHORING STUB: not yet classified.
(@def window-dedicated-p (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-dedicated-p (window: Todo flag: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-prev-buffers (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-prev-buffers (window: Todo prev_buffers: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-next-buffers (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-next-buffers (window: Todo next_buffers: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-parameters (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-parameter (window: Todo parameter: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-parameter (window: Todo parameter: Todo value: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-display-table (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-display-table (window: Todo table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def next-window (&optional window: Todo minibuf: Todo all_frames: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def previous-window (&optional window: Todo minibuf: Todo all_frames: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-list (&optional frame: Todo minibuf: Todo window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-list-1 (&optional window: Todo minibuf: Todo all_frames: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-buffer-window (&optional buffer_or_name: Todo all_frames: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def delete-other-windows-internal (&optional window: Todo root: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def run-window-configuration-change-hook (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def run-window-scroll-functions (&optional window: Todo) Todo))

;;; ============================================================
;;; Window changes and scrolling

(et-declare
;; AUTHORING STUB: not yet classified.
(@def set-window-buffer (window: Todo buffer_or_name: Todo &optional keep_margins: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def force-window-update (&optional object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-new-pixel (window: Todo size: Todo &optional add: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-new-total (window: Todo size: Todo &optional add: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-new-normal (window: Todo &optional size: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-resize-apply (&optional frame: Todo horizontal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-resize-apply-total (&optional frame: Todo horizontal: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def split-window-internal (old: Todo pixel_size: Todo side: Todo normal_size: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def delete-window-internal (window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def resize-mini-window-internal (window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def scroll-up (&optional arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def scroll-down (&optional arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def other-window-for-scrolling () Todo)
;; AUTHORING STUB: not yet classified.
(@def scroll-left (&optional arg: Todo set_minimum: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def scroll-right (&optional arg: Todo set_minimum: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def minibuffer-selected-window () Todo)
;; AUTHORING STUB: not yet classified.
(@def recenter (&optional arg: Todo redisplay: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-text-width (&optional window: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-text-height (&optional window: Todo pixelwise: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def move-to-window-line (arg: Todo) Todo))

;;; ============================================================
;;; Window configurations and appearance

(et-declare
;; AUTHORING STUB: not yet classified.
(@def window-configuration-p (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-configuration-frame (config: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-configuration (configuration: Todo &optional dont_set_frame: Todo dont_set_miniwindow: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def current-window-configuration (&optional frame: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-margins (window: Todo left_width: Todo &optional right_width: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-margins (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-fringes (window: Todo left_width: Todo &optional right_width: Todo outside_margins: Todo persistent: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-fringes (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-cursor-type (window: Todo type: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-cursor-type (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-scroll-bars (window: Todo &optional width: Todo vertical_type: Todo height: Todo horizontal_type: Todo persistent: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-scroll-bars (&optional window: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-vscroll (&optional window: Todo pixels_p: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-window-vscroll (window: Todo vscroll: Todo &optional pixels_p: Todo preserve_vscroll_p: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def window-configuration-equal-p (x: Todo y: Todo) Todo))

;;; ============================================================
