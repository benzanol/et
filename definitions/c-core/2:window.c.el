;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Window predicates and relationships

(et-declare
 (@def windowp (object: Any) Boolean)
 (@def window-valid-p (object: Any) Boolean)
 (@def window-live-p (object: Any) Boolean)
 (@def window-frame (&optional window: Window?) Frame)
 (@def frame-root-window (&optional frame_or_window: Frame|Window?) Window)
 (@def minibuffer-window (&optional frame: Frame?) Window)
 (@def window-minibuffer-p (&optional window: Window?) Boolean)
 (@def frame-first-window (&optional frame_or_window: Frame|Window?) Window)
 (@def frame-selected-window (&optional frame_or_window: Frame|Window?) Window)
 ;; The per-frame field is zero-initialized and only populated the first
 ;; time window-change-record runs for that frame, so it is genuinely
 ;; nilable for a freshly created frame.
 (@def frame-old-selected-window (&optional frame: Frame?) Window?)
 (@def set-frame-selected-window (frame: Frame? window: Window &optional norecord: Bool) Window)
 (@def selected-window () Window)
 (@def old-selected-window () Window)
 (@def select-window (window: Window &optional norecord: Bool) Window)
 (@def window-buffer (&optional window: Window?) Buffer?)
 (@def window-old-buffer (&optional window: Window?) Buffer|Boolean)
 (@def window-parent (&optional window: Window?) Window?)
 (@def window-top-child (&optional window: Window?) Window?)
 (@def window-left-child (&optional window: Window?) Window?)
 (@def window-next-sibling (&optional window: Window?) Window?)
 (@def window-prev-sibling (&optional window: Window?) Window?)
 (@def window-combination-limit (window: Window) Bool)
 (@def set-window-combination-limit (window: Window limit: Bool) Bool)
 (@def window-use-time (&optional window: Window?) Integer)
 (@def window-bump-use-time (&optional window: Window?) Integer?))


;;; ============================================================
;;; Window dimensions

(et-declare
 (@def window-pixel-width (&optional window: Window?) Integer)
 (@def window-pixel-height (&optional window: Window?) Integer)
 (@def window-old-pixel-width (&optional window: Window?) Integer)
 (@def window-old-pixel-height (&optional window: Window?) Integer)
 (@def window-total-height (&optional window: Window? round: Any) Integer)
 (@def window-total-width (&optional window: Window? round: Any) Integer)
 (@def window-new-total (&optional window: Window?) Integer)
 (@def window-normal-size (&optional window: Window? horizontal: Bool) Number)
 (@def window-new-normal (&optional window: Window?) Any)
 (@def window-new-pixel (&optional window: Window?) Integer)
 (@def window-pixel-left (&optional window: Window?) Integer)
 (@def window-pixel-top (&optional window: Window?) Integer)
 (@def window-left-column (&optional window: Window?) Integer)
 (@def window-top-line (&optional window: Window?) Integer)
 (@def window-body-width (&optional window: Window? pixelwise: Any) Integer)
 (@def window-body-height (&optional window: Window? pixelwise: Any) Integer)
 (@def window-old-body-pixel-width (&optional window: Window?) Integer)
 (@def window-old-body-pixel-height (&optional window: Window?) Integer)
 (@def window-mode-line-height (&optional window: Window?) Integer)
 (@def window-header-line-height (&optional window: Window?) Integer)
 (@def window-tab-line-height (&optional window: Window?) Integer)
 (@def window-right-divider-width (&optional window: Window?) Integer)
 (@def window-bottom-divider-width (&optional window: Window?) Integer)
 (@def window-scroll-bar-width (&optional window: Window?) Integer)
 (@def window-scroll-bar-height (&optional window: Window?) Integer)
 (@def window-hscroll (&optional window: Window?) Integer)
 (@def set-window-hscroll (window: Window? ncol: Integer) Integer)
 (@def coordinates-in-window-p
       (coordinates: &Cons<Number~Number> window: Window?)
       Cons<Number~Number>|@mode-line|@vertical-line|@header-line|@tab-line|@left-fringe|@right-fringe|@left-margin|@right-margin|@right-divider|@bottom-divider?)
 (@def window-at (x: Integer y: Integer &optional frame: Frame?) Window?))


;;; ============================================================
;;; Window positions and display

(et-declare
 (@def window-point (&optional window: Window?) Integer)
 (@def window-old-point (&optional window: Window?) Integer)
 (@def window-start (&optional window: Window?) Integer)
 (@def window-end (&optional window: Window? update: Bool) Integer)
 (@def set-window-point ([(<= P IntOrMarker)] window: Window? pos: P) P)
 (@def set-window-start ([(<= P IntOrMarker)] window: Window? pos: P &optional noforce: Bool) P)
 (@def pos-visible-in-window-p
       (&optional pos: IntOrMarker|@t? window: Window? partially: Bool)
       Boolean|Tuple<Integer~Integer>|Tuple<Integer~Integer~Integer~Integer~Integer~Integer>)
 (@def window-line-height
       (&optional line: Integer|@header-line|@mode-line? window: Window?)
       Tuple<Integer~Integer~Integer~Integer>?)
 (@def window-lines-pixel-dimensions
       (&optional window: Window? first: Integer? last: Integer?
        body: Bool inverse: Bool left: Bool)
       List<Cons<Integer~Integer>>))


;;; ============================================================
;;; Window properties and traversal

(et-declare
 (@def window-dedicated-p (&optional window: Window?) Bool)
 (@def set-window-dedicated-p (window: Window? flag: Bool) Bool)
 (@def window-prev-buffers (&optional window: Window?) List<Tuple<Buffer~Integer~Integer>>)
 (@def set-window-prev-buffers
       (window: Window? prev_buffers: List<Tuple<Buffer~Integer~Integer>>)
       List<Tuple<Buffer~Integer~Integer>>)
 (@def window-next-buffers (&optional window: Window?) List<Buffer>)
 (@def set-window-next-buffers (window: Window? next_buffers: List<Buffer>) List<Buffer>)
 (@def window-parameters (&optional window: Window?) Alist<Any~Any>)
 (@def window-parameter (window: Window? parameter: Any) Any)
 (@def set-window-parameter (window: Window? parameter: Any value: Any) Any)
 (@def window-display-table (&optional window: Window?) Any)
 (@def set-window-display-table (window: Window? table: Any) Any)
 (@def next-window (&optional window: Window? minibuf: Any all_frames: Any) Window)
 (@def previous-window (&optional window: Window? minibuf: Any all_frames: Any) Window)
 (@def window-list (&optional frame: Frame? minibuf: Any window: Window?) List<Window>)
 (@def window-list-1 (&optional window: Window? minibuf: Any all_frames: Any) List<Window>)
 (@def get-buffer-window (&optional buffer_or_name: Buffer|String? all_frames: Any) Window?)
 (@def window-discard-buffer-from-window (buffer: Buffer window: Window &optional all: Bool) Nil)
 (@def delete-other-windows-internal (&optional window: Window? root: Window?) Nil)
 (@def run-window-configuration-change-hook (&optional frame: Frame?) Nil)
 (@def run-window-scroll-functions (&optional window: Window?) Nil))


;;; ============================================================
;;; Window changes and scrolling

(et-declare
 (@def set-window-buffer
       (window: Window? buffer_or_name: Buffer|String &optional keep_margins: Bool)
       Nil)
 (@def force-window-update (&optional object: Window|Buffer|String?) Boolean)
 (@def set-window-new-pixel (window: Window? size: Integer &optional add: Bool) Integer)
 (@def set-window-new-total (window: Window? size: Integer &optional add: Bool) Integer)
 (@def set-window-new-normal (window: Window? &optional size: Any) Any)
 (@def window-resize-apply (&optional frame: Frame? horizontal: Bool) Boolean)
 (@def window-resize-apply-total (&optional frame: Frame? horizontal: Bool) True)
 (@def combine-windows (first: Window? last: Window?) Window?)
 (@def uncombine-window (window: Window?) Boolean)
 (@def split-window-internal
       (old: Window? pixel_size: Integer side: Any normal_size: Any
        &optional refer: Window|Cons<Window~Window|@t>?)
       Window)
 (@def delete-window-internal (window: Window?) Nil)
 (@def resize-mini-window-internal (window: Window) True)
 (@def scroll-up (&optional arg: Integer|@-?) Nil)
 (@def scroll-down (&optional arg: Integer|@-?) Nil)
 (@def other-window-for-scrolling () Window)
 (@def scroll-left (&optional arg: Any set_minimum: Bool) Integer)
 (@def scroll-right (&optional arg: Any set_minimum: Bool) Integer)
 (@def minibuffer-selected-window () Window?)
 (@def recenter (&optional arg: Any redisplay: Bool) Nil)
 (@def window-text-width (&optional window: Window? pixelwise: Bool) Integer)
 (@def window-text-height (&optional window: Window? pixelwise: Bool) Integer)
 (@def move-to-window-line (arg: Any) Integer))


;;; ============================================================
;;; Window configurations and appearance

(et-declare
 (@def window-configuration-p (object: Any) Boolean)
 (@def window-configuration-frame (config: WindowConfiguration) Frame)
 (@def set-window-configuration
       (configuration: WindowConfiguration
        &optional dont_set_frame: Bool dont_set_miniwindow: Bool)
       Boolean)
 (@def current-window-configuration (&optional frame: Frame?) WindowConfiguration)
 (@def set-window-margins (window: Window? left_width: Integer? &optional right_width: Integer?) Boolean)
 (@def window-margins (&optional window: Window?) Cons<Integer?~Integer?>)
 (@def set-window-fringes
       (window: Window? left_width: Integer?
        &optional right_width: Integer? outside_margins: Bool persistent: Bool)
       Boolean)
 (@def window-fringes (&optional window: Window?) Tuple<Integer~Integer~Boolean~Boolean>)
 (@def set-window-cursor-type
       (window: Window? type: @t|@box|@hollow|@bar|@hbar|Cons<@box|@bar|@hbar~Integer>?)
       @t|@box|@hollow|@bar|@hbar|Cons<@box|@bar|@hbar~Integer>?)
 (@def window-cursor-type (&optional window: Window?) @t|@box|@hollow|@bar|@hbar|Cons<@box|@bar|@hbar~Integer>?)
 (@def window-cursor-info (&optional window: Window?) Nil|Vector<Any>)
 (@def set-window-scroll-bars
       (window: Window? &optional width: Integer? vertical_type: @left|@right|@t?
        height: Integer? horizontal_type: @bottom|@t? persistent: Bool)
       Boolean)
 (@def window-scroll-bars
       (&optional window: Window?)
       Tuple<Integer?~Integer~@left|@right|@t?~Integer?~Integer~@bottom|@t?~Boolean>)
 (@def window-vscroll (&optional window: Window? pixels_p: Bool) Number)
 (@def set-window-vscroll
       (window: Window? vscroll: Number &optional pixels_p: Bool preserve_vscroll_p: Bool)
       Number)
 (@def window-configuration-equal-p (x: WindowConfiguration y: WindowConfiguration) Boolean))

;;; ============================================================
