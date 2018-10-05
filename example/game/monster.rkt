#lang typed/racket

(provide monster-die)

(require racket/format

         "../../color.rkt"

         "message-log.rkt"
         "types.rkt")

(: monster-die (-> GameObject GameObject))
(define (monster-die object)
  (message-add (format "~a is dead!" (game-object-name object)))
  (struct-copy game-object object
               [char #\%]
               [color color-dark-red]
               [alive? #f] [blocks #f]
               [name (format "Remains of ~a" (game-object-name object))]))
