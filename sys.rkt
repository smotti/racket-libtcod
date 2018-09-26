#lang typed/racket

(module ffi-sys racket
  (provide event?
           sys-wait-for-event)

  (require ffi/unsafe

           "libtcod.rkt"
           (submod "console.rkt" ffi-console)
           (submod "mouse.rkt" ffi-mouse))

  ;;;
  ;;; Types
  ;;;

  (define event-symbols
    (list 'KEY_PRESS
          'KEY_RELEASE
          'KEY
          'MOUSE_MOVE
          'MOUSE_PRESS
          'MOUSE_RELEASE
          'MOUSE
          'KEY_PRESS_MOUSE_MOVE
          'ANY))

  (define (event? event)
    (member event event-symbols))

  (define _event (_enum '(KEY_PRESS = 1
                          KEY_RELEASE = 2
                          KEY = 3
                          MOUSE_MOVE = 4
                          MOUSE_PRESS = 8
                          MOUSE_RELEASE = 16
                          MOUSE = 28
                          KEY_PRESS_MOUSE_MOVE = 5
                          ANY = 31)))

  ;;;
  ;;; Event Handling
  ;;;

  (define-tcod sys-wait-for-event
    (_fun _event [k : (_ptr o _key)] [m : (_ptr o _mouse)] _bool
          -> (e : _event) -> (values e k m))
    #:c-id TCOD_sys_wait_for_event))


(require "console.rkt"
         "mouse.rkt")
(require/typed/provide 'ffi-sys
  [#:opaque Event event?]
  [sys-wait-for-event (-> Symbol Boolean (Values Symbol Key Mouse))]
  )
