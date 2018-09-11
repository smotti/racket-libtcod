#lang racket

(provide sys-wait-for-event)

(require ffi/unsafe

         "libtcod.rkt"
         "console.rkt"
         "mouse.rkt")

;;;
;;; Types
;;;

(define _event (_enum '(KEY_PRESS = 1
                        KEY_RELEASE = 2
                        KEY = 3
                        MOUSE_MOVE = 4
                        MOUSE_PRESS = 8
                        MOUSE_RELEASE = 16
                        MOUSE = 28
                        ANY = 31)))

;;;
;;; Event Handling
;;;

(define-tcod sys-wait-for-event
  (_fun _event [k : (_ptr o _key)] [m : (_ptr o _mouse)] _bool
        -> (e : _event) -> (values e k m))
  #:c-id TCOD_sys_wait_for_event)
