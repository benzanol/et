;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Frame creation and focus handling

(et-declare
 (@def frame-creation-function (params: Alist<Symbol~Any>) Frame)
 (@variable window-system-default-frame-alist Alist<Symbol~Alist<Symbol~Any>>)
 (@variable display-format-alist Alist<String~Symbol>)
 (@variable initial-frame-alist Alist<Symbol~Any>)
 (@variable minibuffer-frame-alist Alist<Symbol~Any>)
 (@def handle-delete-frame (event: Any) Any)
 (@def frame-focus-state (&optional frame: Frame?) Boolean|@unknown)
 (@variable after-focus-change-function fn<Nil~Any>)
 (@variable focus-in-hook List<fn<Nil~Any>>)
 (@variable focus-out-hook List<fn<Nil~Any>>)
 (@def handle-focus-in (event: Any) Any)
 (@def handle-focus-out (event: Any) Any)
 (@def handle-move-frame (event: Any) Any))

;;; ============================================================
;;; Arrangement of frames at startup

(et-declare
 (@variable frame-initial-frame Frame?)
 (@variable frame-initial-frame-alist Alist<Symbol~Any>?)
 (@variable frame-initial-geometry-arguments Alist<Symbol~Any>?)
 (@def frame-initialize () Any)
 (@variable frame-notice-user-settings Boolean)
 (@def tool-bar-lines-needed (&optional frame: Frame? pixelwise: Bool) Integer)
 (@def frame-notice-user-settings () Nil)
 (@def make-initial-minibuffer-frame (display: String?) Frame))

;;; ============================================================
;;; Creation of additional frames, and other frame miscellanea

(et-declare
 (@def modify-all-frames-parameters (alist: Alist<Symbol~Any>) Alist<Symbol~Any>)
 (@def get-other-frame () Frame)
 (@def next-window-any-frame () Any)
 (@def previous-window-any-frame () Any)
 (@def next-multiframe-window () Any)
 (@def previous-multiframe-window () Any)
 (@def window-system-for-display (display: String) Symbol?)
 (@def make-frame-on-display (display: String &optional parameters: Alist<Symbol~Any>?) Frame)
 (@def make-frame-on-current-monitor (&optional parameters: Alist<Symbol~Any>?) Frame)
 (@def make-frame-on-monitor (monitor: String &optional display: String? parameters: Alist<Symbol~Any>?) Frame)
 (@def close-display-connection (display: String) Any)
 (@def make-frame-command () Frame)
 (@def clone-frame (&optional frame: Frame? no-windows: Bool) Frame)
 (@variable before-make-frame-hook List<fn<Nil~Any>>)
 (@variable after-make-frame-functions List<fn1<Frame>>)
 (@variable after-setting-font-hook List<fn<Nil~Any>>)
 (@variable frame-inherited-parameters List<Symbol>)
 (@def make-frame (&optional parameters: Alist<Symbol~Any>?) Frame)
 (@def filtered-frame-list (predicate: fn1<Frame~Bool>) List<Frame>)
 (@def minibuffer-frame-list () List<Frame>)
 (@def get-device-terminal (device: Terminal|Frame|String?) Terminal)
 (@def frames-on-display-list (&optional device: Terminal|Frame|String?) List<Frame>)
 (@def framep-on-display (&optional terminal: Terminal|Frame|String?) Symbol)
 (@def frame-remove-geometry-params (param-list: Alist<Symbol~Any>) Alist<Symbol~Any>)
 (@def select-frame-set-input-focus (frame: Frame &optional norecord: Bool) Any)
 (@def other-frame (arg: Integer) Any)
 (@def other-frame-prefix () String)
 (@def iconify-or-deiconify-frame () Any)
 (@def suspend-frame () Any)
 (@def make-frame-names-alist () Alist<String?~Frame>)
 (@variable frame-name-history List<String>)
 (@def select-frame-by-name (name: String) Any))

;;; ============================================================
;;; Background mode

(et-declare
 (@variable frame-background-mode @dark|@light?)
 (@variable inhibit-frame-set-background-mode Boolean)
 (@def frame-set-background-mode (frame: Frame &optional keep-face-specs: Bool) Any)
 (@def frame-terminal-default-bg-mode (frame: Frame) Symbol))

;;; ============================================================
;;; Frame configurations

(et-declare
 (@def current-frame-configuration ()
       (Cons @frame-configuration (List (Tuple Frame (Alist Symbol Any) WindowConfiguration))))
 (@def set-frame-configuration
       (configuration: (Cons @frame-configuration (List (Tuple Frame (Alist Symbol Any) WindowConfiguration)))
        &optional nodelete: Bool)
       List<Frame>))

;;; ============================================================
;;; Convenience functions for accessing and interactively changing frame parameters

(et-declare
 (@def frame-height (&optional frame: Frame?) Integer)
 (@def frame-width (&optional frame: Frame?) Integer)
 (@def frame-border-width (&optional frame: Frame?) Integer)
 (@def frame-pixel-width (&optional frame: Frame?) Integer)
 (@def frame-pixel-height (&optional frame: Frame?) Integer)
 (@def frame-inner-width (&optional frame: Frame?) Integer)
 (@def frame-inner-height (&optional frame: Frame?) Integer)
 (@def frame-outer-width (&optional frame: Frame?) Integer)
 (@def frame-outer-height (&optional frame: Frame?) Integer)
 (@def set-frame-font
       (font: String|Font &optional keep-size: Bool frames: Nil|True|List<Frame> inhibit-customize: Bool)
       Nil)
 (@def set-frame-parameter (frame: Frame? parameter: Symbol value: Any) Any)
 (@def set-background-color (color-name: String) Any)
 (@def set-foreground-color (color-name: String) Any)
 (@def set-cursor-color (color-name: String) Any)
 (@def set-mouse-color (color-name: String?) Any)
 (@def set-border-color (color-name: String) Any)
 (@def auto-raise-mode (&optional arg: Integer|@toggle?) Any)
 (@def auto-lower-mode (&optional arg: Integer|@toggle?) Any)
 (@def set-frame-name (name: String) Any)
 (@def frame-current-scroll-bars (&optional frame: Frame?) Cons<@left|@right?~@bottom?>)
 (@def frame-geometry (&optional frame: Frame?) Alist<Symbol~Any>)
 (@def frame-edges (&optional frame: Frame? type: @outer-edges|@native-edges|@inner-edges?)
       (Tuple Integer Integer Integer Integer))
 (@def mouse-absolute-pixel-position () Cons<Integer~Integer>)
 (@def set-mouse-absolute-pixel-position (x: Integer y: Integer) Any)
 (@def frame-monitor-attributes (&optional frame: Frame?) Alist<Symbol~Any>)
 (@def frame-monitor-attribute (attribute: Symbol &optional frame: Frame? x: Integer? y: Integer?) Any)
 (@def frame-monitor-geometry (&optional frame: Frame? x: Integer? y: Integer?)
       (or (Tuple Integer Integer Integer Integer) Nil))
 (@def frame-monitor-workarea (&optional frame: Frame? x: Integer? y: Integer?)
       (or (Tuple Integer Integer Integer Integer) Nil))
 (@def frame-list-z-order (&optional display: Terminal|Frame|String?) List<Frame>)
 (@def frame-restack (frame1: Frame frame2: Frame &optional above: Bool) Any)
 (@def frame-size-changed-p (&optional frame: Frame?) Boolean))

;;; ============================================================
;;; Frame and display capabilities

(et-declare
 (@def display-mouse-p (&optional display: Terminal|Frame|String?) Bool)
 (@def display-popup-menus-p (&optional display: Terminal|Frame|String?) Bool)
 (@def display-graphic-p (&optional display: Terminal|Frame|String?) Boolean)
 (@def display-images-p (&optional display: Terminal|Frame|String?) Bool)
 (@def display-blink-cursor-p (&optional display: Terminal|Frame|String?) Boolean)
 (@def display-multi-frame-p (&optional display: Terminal|Frame|String?) Boolean)
 (@def display-multi-font-p (&optional display: Terminal|Frame|String?) Boolean)
 (@variable tty-select-active-regions Boolean)
 (@def display-selections-p (&optional display: Terminal|Frame|String?) Bool)
 (@def display-symbol-keys-p (&optional display: Terminal|Frame|String?) Bool)
 (@def display-screens (&optional display: Terminal|Frame|String?) Integer)
 (@def display-pixel-height (&optional display: Terminal|Frame|String?) Integer)
 (@def display-pixel-width (&optional display: Terminal|Frame|String?) Integer)
 (@variable display-mm-dimensions-alist Alist<String|True~Cons<Integer~Integer>>)
 (@def display-mm-height (&optional display: Terminal|Frame|String?) Integer?)
 (@def display-mm-width (&optional display: Terminal|Frame|String?) Integer?)
 (@def display-backing-store (&optional display: Terminal|Frame|String?) Boolean|@not-useful)
 (@def display-save-under (&optional display: Terminal|Frame|String?) Boolean|@not-useful)
 (@def display-planes (&optional display: Terminal|Frame|String?) Integer)
 (@def display-color-cells (&optional display: Terminal|Frame|String?) Integer)
 (@def display-visual-class (&optional display: Terminal|Frame|String?)
       @static-gray|@gray-scale|@static-color|@pseudo-color|@true-color|@direct-color)
 (@def display-monitor-attributes-list (&optional display: Terminal|Frame|String?) List<Alist<Symbol~Any>>)
 (@def device-class (frame: Frame|Terminal|String? name: String?) Symbol))

;;; ============================================================
;;; On-screen keyboard management

(et-declare
 (@def frame-toggle-on-screen-keyboard (frame: Frame hide: Bool) Boolean))

;;; ============================================================
;;; Frame geometry values

(et-declare
 (@def frame-geom-value-cons
       (type: @top|@left value: (or Integer (Tuple @+ Integer) (Tuple @- Integer)) &optional frame: Frame?)
       (Tuple @+ Integer))
 (@def frame-geom-spec-cons
       (spec: (Cons (or @top @left) (or Integer (Tuple @+ Integer) (Tuple @- Integer))) &optional frame: Frame?)
       (Cons (or @top @left) (Tuple @+ Integer)))
 (@def delete-other-frames (&optional frame: Frame? iconify: Bool) Nil)
 (@variable undelete-frame-mode Boolean)
 (@def undelete-frame-mode (&optional arg: Integer|@toggle?) Boolean)
 (@def undelete-frame (&optional arg: @-|Integer?) Frame))

;;; ============================================================
;;; Window dividers

(et-declare
 (@variable window-divider-default-places @bottom-only|@right-only|True)
 (@def window-divider-width-valid-p (value: Any) Boolean)
 (@variable window-divider-default-bottom-width Positive)
 (@variable window-divider-default-right-width Positive)
 (@def window-divider-mode-apply (enable: Bool) Alist<Symbol~Any>?)
 (@variable window-divider-mode Boolean)
 (@def window-divider-mode (&optional arg: Integer|@toggle?) Boolean))

;;; ============================================================
;;; Blinking cursor

(et-declare
 (@variable blink-cursor-idle-timer *timer?)
 (@variable blink-cursor-timer *timer?)
 (@variable blink-cursor-delay Number)
 (@variable blink-cursor-interval Number)
 (@variable blink-cursor-blinks Integer)
 (@variable blink-cursor-blinks-done Integer)
 (@def blink-cursor-start () Any)
 (@def blink-cursor-timer-function () Nil)
 (@def blink-cursor-end () Nil)
 (@def blink-cursor-suspend () Nil)
 (@def blink-cursor-check () Boolean)
 (@variable blink-cursor-mode Boolean)
 (@def blink-cursor-mode (&optional arg: Integer|@toggle?) Boolean))

;;; ============================================================
;;; Frame maximization and fullscreen

(et-declare
 (@def toggle-frame-maximized (&optional frame: Frame?) Any)
 (@def toggle-frame-fullscreen (&optional frame: Frame?) Any))

;;; ============================================================
;;; Miscellaneous

(et-declare
 (@def frame-hide-title-bar-when-maximized (frame: Frame) Any))

;;; ============================================================
