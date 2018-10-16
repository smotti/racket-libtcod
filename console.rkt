#lang typed/racket

(module ffi-console racket
  (provide _key
           console?
           console-blit
           ;         console-disable-keyboard-repeat
           console-clear
           console-get-height-rect
           console-flush
           console-init-root
           console-is-key-pressed
           console-is-window-closed
           console-new
           console-print-ex
           console-print-rect-ex
           console-put-char
           console-put-char-ex
           console-rect
           console-root
           console-set-char-background
           console-set-default-background
           console-set-default-foreground
           ;         console-set-keyboard-repeat
           console-set-window-title
           console-wait-for-keypress
           key?
           key-c
           key-lalt
           key-lmeta
           key-lctrl
           key-ralt
           key-rmeta
           key-rctrl
           key-shift
           key-txt
           key-vk
           make-key-default
           )

  (require ffi/unsafe

           "libtcod.rkt"
           (submod "color.rkt" ffi-color))

  ;;;
  ;;; Constants
  ;;;

  (define console-root 0)

  ;;;
  ;;; Types
  ;;;

  (define _char (make-ctype _int char->integer integer->char))

  (define _renderer (_enum '(RENDERER_GLSL
                             RENDERER_OPENGL
                             RENDERER_SDL
                             NB_RENDERERS)))

  (define-cpointer-type _console)

  (define _keycode (_enum '(NONE = 0
                            ESCAPE = 1
                            BACKSPACE = 2
                            TAB = 3
                            ENTER = 4
                            SHIFT = 5
                            CONTROL = 6
                            ALT = 7
                            PAUSE = 8
                            CAPSLOCK = 9
                            PAGEUP = 10
                            PAGEDOWN = 11
                            END = 12
                            HOME = 13
                            UP = 14
                            LEFT = 15
                            RIGHT = 16
                            DOWN = 17
                            PRINTSCREEN = 18
                            INSERT = 19
                            DELETE = 20
                            LWIN = 21
                            RWIN = 22
                            APPS = 23
                            NO_0 = 24
                            NO_1 = 25
                            NO_2 = 26
                            NO_3 = 27
                            NO_4 = 28
                            NO_5 = 29
                            NO_6 = 30
                            NO_7 = 31
                            NO_8 = 32
                            NO_9 = 33
                            KP0 = 34
                            KP1 = 35
                            KP2 = 36
                            KP3 = 37
                            KP4 = 38
                            KP5 = 39
                            KP6 = 40
                            KP7 = 41
                            KP8 = 42
                            KP9 = 43
                            KPADD = 44
                            KPSUB = 45
                            KPDIV = 46
                            KPMUL = 47
                            KPDEC = 48
                            KPENTER = 49
                            F1 = 50
                            F2 = 51
                            F3 = 52
                            F4 = 53
                            F5 = 54
                            F6 = 55
                            F7 = 56
                            F8 = 57
                            F9 = 58
                            F10 = 59
                            F11 = 60
                            F12 = 61
                            NUMLOCK = 62
                            SCROLLLOCK = 63
                            SPACE = 64
                            CHAR = 65
                            TEXT = 66)))

  (define-cstruct _key ([vk _keycode]
                        [c _char]
                        [txt _string]
                        [pressed _bool]
                        [lalt _bool]
                        [lctrl _bool]
                        [lmeta _bool]
                        [ralt _bool]
                        [rctrl _bool]
                        [rmeta _bool]
                        [shift _bool]))
  (define (make-key-default)
    (make-key 'CHAR #\@ "@" #f #f #f #f #f #f #f #f))

  (define _alignment (_enum '(LEFT RIGHT CENTER)))

  (define _bkgnd_flag (_enum '(BKGND_NONE
                               BKGND_SET
                               BKGND_MULTIPLY
                               BKGND_LIGHTEN
                               BKGND_DARKEN
                               BKGND_SCREEN
                               BKGND_COLOR_DODGE
                               BKGND_COLOR_BURN
                               BKGND_ADD
                               BKGND_ADDA
                               BKGND_BURN
                               BKGND_OVERLAY
                               BKGND_ALPH
                               BKGND_DEFAULT)))

  ;;;
  ;;; Console
  ;;;

  (define-tcod console-blit
    (_fun (src x-src y-src w-src h-src dst x-dst y-dst [fg 1.0] [bg 1.0]) ::
          (src : _console) (x-src : _int) (y-src : _int) (w-src : _int) (h-src : _int)
          (dst : _int) (x-dst : _int) (y-dst : _int)
          (fg : _float) (bg : _float)
          -> _void)
    #:c-id TCOD_console_blit)

  (define-tcod console-clear
    (_fun _console -> _void)
    #:c-id TCOD_console_clear)

  (define-tcod console-get-height-rect
    (_fun _console _int _int _int _int _string -> _int)
    #:c-id TCOD_console_get_height_rect)

  (define-tcod console-flush
    (_fun -> _void)
    #:c-id TCOD_console_flush)

  (define-tcod console-init-root
    (_fun _int _int _string _bool _renderer -> _void)
    #:c-id TCOD_console_init_root)

  (define-tcod console-is-window-closed
    (_fun -> _bool)
    #:c-id TCOD_console_is_window_closed)

  (define-tcod console-new
    (_fun _int _int -> _console)
    #:c-id TCOD_console_new)

  (define-tcod console-print-ex
    (_fun _console _int _int _bkgnd_flag _alignment _string -> _void)
    #:c-id TCOD_console_print_ex)

  (define-tcod console-print-rect-ex
    (_fun _console _int _int _int _int _bkgnd_flag _alignment _string -> _int)
    #:c-id TCOD_console_print_rect_ex)

  (define-tcod console-put-char
    (_fun _console _int _int _char _bkgnd_flag -> _void)
    #:c-id TCOD_console_put_char)

  (define-tcod console-put-char-ex
    (_fun _console _int _int _char _color _color -> _void)
    #:c-id TCOD_console_put_char_ex)

  (define-tcod console-rect
    (_fun _console _int _int _int _int _bool _bkgnd_flag -> _void)
    #:c-id TCOD_console_rect)

  (define-tcod console-set-char-background
    (_fun _console _int _int _color _bkgnd_flag -> _void)
    #:c-id TCOD_console_set_char_background)

  (define-tcod console-set-default-background
    (_fun _console _color -> _void)
    #:c-id TCOD_console_set_default_background)

  (define-tcod console-set-default-foreground
    (_fun _console _color -> _void)
    #:c-id TCOD_console_set_default_foreground)

  (define-tcod console-set-window-title
    (_fun _string -> _void)
    #:c-id TCOD_console_set_window_title)

  ;;;
  ;;; Keyboard Input
  ;;;

  ;(define-tcod console-disable-keyboard-repeat
  ;  (_fun -> _void)
  ;  #:c-id TCOD_console_disable_keyboard_repeat)

  ;(define-tcod console-set-keyboard-repeat
  ;  (_fun _int _int -> _void)
  ;  #:c-id TCOD_console_set_keyboard_repeat)

  (define-tcod console-wait-for-keypress
    (_fun _bool -> _key)
    #:c-id TCOD_console_wait_for_keypress)

  (define-tcod console-is-key-pressed
    (_fun _keycode -> _bool)
    #:c-id TCOD_console_is_key_pressed))


(require "color.rkt")
(require/typed/provide 'ffi-console
  [#:opaque Console console?]
  [#:opaque Key key?]
  [console-blit (->* (Console Integer Integer Integer Integer Integer Integer Integer) (Float Float) Void)]
  [console-clear (-> Console Void)]
  [console-get-height-rect (-> Console Integer Integer Integer Integer String Integer)]
  [console-flush (-> Void)]
  [console-init-root (-> Integer Integer String Boolean Symbol Void)]
  [console-is-key-pressed (-> Symbol Boolean)]
  [console-is-window-closed (-> Boolean)]
  [console-new (-> Integer Integer Console)]
  [console-print-ex (-> Console Integer Integer Symbol Symbol String Void)]
  [console-print-rect-ex (-> Console Integer Integer Integer Integer Symbol Symbol String Integer)]
  [console-put-char (-> Console Integer Integer Char Symbol Void)]
  [console-put-char-ex (-> Console Integer Integer Char Color Color Void)]
  [console-rect (-> Console Integer Integer Integer Integer Boolean Symbol Void)]
  [console-root Integer]
  [console-set-char-background (-> Console Integer Integer Color Symbol Void)]
  [console-set-default-background (-> Console Color Void)]
  [console-set-default-foreground (-> Console Color Void)]
  [console-set-window-title (-> String Void)]
  [console-wait-for-keypress (-> Boolean Key)]
  [key-c (-> Key Char)]
  [key-lalt (-> Key Boolean)]
  [key-lctrl (-> Key Boolean)]
  [key-lmeta (-> Key Boolean)]
  [key-ralt (-> Key Boolean)]
  [key-rctrl (-> Key Boolean)]
  [key-rmeta (-> Key Boolean)]
  [key-shift (-> Key Boolean)]
  [key-vk (-> Key Symbol)]
  [make-key-default (-> Key)]
  )
