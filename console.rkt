#lang typed/racket

(module ffi-console racket
  (provide _key
           console?
           console-blit
           ;         console-disable-keyboard-repeat
           console-clear
           console-flush
           console-init-root
           console-is-key-pressed
           console-is-window-closed
           console-new
           console-print-ex
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

  (define _keycode (_enum '(NONE
                            ESCAPE
                            BACKSPACE
                            TAB
                            ENTER
                            SHIFT
                            CONTROL
                            ALT
                            PAUSE
                            CAPSLOCK
                            PAGEUP
                            PAGEDOWN
                            END
                            HOME
                            UP
                            LEFT
                            RIGHT
                            DOWN
                            PRINTSCREEN
                            INSERT
                            DELETE
                            LWIN
                            RWIN
                            APPS
                            NO_0
                            NO_1
                            NO_2
                            NO_3
                            NO_4
                            NO_5
                            NO_6
                            NO_7
                            NO_8
                            NO_9
                            KP0
                            KP1
                            KP2
                            KP3
                            KP4
                            KP5
                            KP6
                            KP7
                            KP8
                            KP9
                            KPADD
                            KPSUB
                            KPDIV
                            KPMUL
                            KPDEC
                            KPENTER
                            F1
                            F2
                            F3
                            F4
                            F5
                            F6
                            F7
                            F8
                            F9
                            F10
                            F11
                            F12
                            NUMLOCK
                            SCROLLLOCK
                            SPACE
                            CHAR)))

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
    (_fun _console _int _int _int _int _int _int _int (_float = 1.0) (_float = 1.0)
          -> _void)
    #:c-id TCOD_console_blit)

  (define-tcod console-clear
    (_fun _console -> _void)
    #:c-id TCOD_console_clear)

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
  [console-blit (-> Console Integer Integer Integer Integer Integer Integer Integer Void)]
  [console-clear (-> Console Void)]
  [console-flush (-> Void)]
  [console-init-root (-> Integer Integer String Boolean Symbol Void)]
  [console-is-key-pressed (-> Symbol Boolean)]
  [console-is-window-closed (-> Boolean)]
  [console-new (-> Integer Integer Console)]
  [console-print-ex (-> Console Integer Integer Symbol Symbol String Void)]
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
