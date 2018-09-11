#lang racket

(provide random-default-get-int
         random-get-int
         )

(require ffi/unsafe

         "libtcod.rkt")

;;;;
;;;; Types
;;;;

(define-cpointer-type _random)

;;;;
;;;; PRNG
;;;;

(define-tcod random-get-int
  (_fun _int _int _int -> _int)
  #:c-id TCOD_random_get_int)

(define (random-default-get-int minimum maximum)
  (random-get-int 0 minimum maximum))
