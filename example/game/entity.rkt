#lang racket

(provide entity-dead?
         entity-die
         entity-move
         make-entity)

(require "../../color.rkt"
         "../../console.rkt"

         "components.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         )

(define (entity-dead? an-entity)
  (not (entity-alive? an-entity)))

(define (entity-die an-entity)
  (message-add (format "~s is dead!" (entity-name an-entity))
               #:color color-orange)
  (struct-copy entity
               an-entity
               [char #\%] [color color-dark-red]
               [blocks #f] [fighter #f] [ai #f]
               [name (format "remains of ~s"
                             (entity-name an-entity))]
               [state 'dead]))

(define (entity-move an-entity
                          #:dx [dx (entity-dx an-entity)]
                          #:dy [dy (entity-dy an-entity)])
  (define-values (to-x to-y) (values (+ (entity-x an-entity) dx)
                                     (+ (entity-y an-entity) dy)))
  (struct-copy entity an-entity
               [x to-x] [y to-y] [dx 0] [dy 0]))
