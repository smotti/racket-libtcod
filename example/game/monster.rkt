#lang racket

(provide monster-die)

(require racket/format

         "../../color.rkt"

         "message-log.rkt"
         "types.rkt")

(define (monster-die an-entity)
  (message-add (format "~a is dead!" (entity-name an-entity)))
  (struct-copy entity an-entity
               [char #\%]
               [color color-dark-red]
               [alive? #f] [blocks #f]
               [name (format "Remains of ~a" (entity-name an-entity))]))
