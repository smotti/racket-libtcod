#lang racket

(provide _key
         console?
         console-blit
         ;         console-disable-keyboard-repeat
         console-flush
         console-init-root
         console-is-key-pressed
         console-is-window-closed
         console-new
         console-put-char
         console-put-char-ex
         console-root
         console-set-char-background
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


(define _bkgnd_flag (_enum '(BKGND_NONE BKGND_SET)))

;;;
;;; Console
;;;

(define-tcod console-init-root
  (_fun _int _int _string _bool _renderer -> _void)
  #:c-id TCOD_console_init_root)

(define-tcod console-blit
  (_fun _console _int _int _int _int _int _int _int (_float = 1.0) (_float = 1.0)
        -> _void)
  #:c-id TCOD_console_blit)

(define-tcod console-is-window-closed
  (_fun -> _bool)
  #:c-id TCOD_console_is_window_closed)

(define-tcod console-new
  (_fun _int _int -> _console)
  #:c-id TCOD_console_new)

(define-tcod console-set-window-title
  (_fun _string -> _void)
  #:c-id TCOD_console_set_window_title)

(define-tcod console-set-default-foreground
  (_fun _console _color -> _void)
  #:c-id TCOD_console_set_default_foreground)

(define-tcod console-put-char
  (_fun _console _int _int _char _bkgnd_flag -> _void)
  #:c-id TCOD_console_put_char)

(define-tcod console-put-char-ex
  (_fun _console _int _int _char _color _color -> _void)
  #:c-id TCOD_console_put_char_ex)

(define-tcod console-set-char-background
 (_fun _console _int _int _color _bkgnd_flag -> _void)
  #:c-id TCOD_console_set_char_background)

(define-tcod console-flush
  (_fun -> _void)
  #:c-id TCOD_console_flush)

;;;
;;; Keyboard Input
;;;
;
;(define-tcod console-disable-keyboard-repeat
;  (_fun -> _void)
;  #:c-id TCOD_console_disable_keyboard_repeat)
;
;(define-tcod console-set-keyboard-repeat
;  (_fun _int _int -> _void)
;  #:c-id TCOD_console_set_keyboard_repeat)
;
(define-tcod console-wait-for-keypress
  (_fun _bool -> _key)
  #:c-id TCOD_console_wait_for_keypress)

(define-tcod console-is-key-pressed
  (_fun _keycode -> _bool)
  #:c-id TCOD_console_is_key_pressed)
