#lang racket

(provide game-object-dead?
         game-object-die
         game-object-move
         make-game-object)

(require "../../color.rkt"
         "../../console.rkt"

         "components.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         )

(define (game-object-dead? obj)
  (not (game-object-alive? obj)))

(define (game-object-die obj)
  (message-add (format "~s is dead!" (game-object-name obj))
               #:color color-orange)
  (struct-copy game-object
               obj
               [char #\%] [color color-dark-red]
               [blocks #f] [fighter #f] [ai #f]
               [name (format "remains of ~s"
                             (game-object-name obj))]
               [state 'dead]))

(define (game-object-move object
                          #:dx [dx (game-object-dx object)]
                          #:dy [dy (game-object-dy object)])
  (define-values (to-x to-y) (values (+ (game-object-x object) dx)
                                     (+ (game-object-y object) dy)))
  (struct-copy game-object object
               [x to-x] [y to-y] [dx 0] [dy 0]))
