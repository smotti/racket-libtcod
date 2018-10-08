#lang racket

(provide entity-dead?
         entity-die
         entity-move
         entity-set-state
         place-entities
         )

(require "../../color.rkt"
         "../../console.rkt"
         "../../random.rkt"

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

(define (entity-set-state an-entity new-state)
  (struct-copy entity an-entity [state new-state]))

; NOTE: This could probably also check the fov map so that entities don't get
; placed too close to the player.
; NOTE: Eagerly trying to place an entity might not the best idea, instead it
; should stop after several failed attempts.
(define (place-entities entities a-map)
  (define (place-entity enty placed)
    (define x (random-default-get-int 0 (- MAP-WIDTH 1)))
    (define y (random-default-get-int 0 (- MAP-HEIGHT 1)))
    (cond [(not (tile-is-blocked? x y a-map placed))
           (struct-copy entity enty [x x] [y y])]
          ; Eagerly try to place the entity
          [else (place-entity enty placed)]))
  (for/fold ([placed '()])
            ([e entities])
    (cons (place-entity e placed) placed)))
