#lang racket

(provide entity-alive?
         entity-dead?
         entity-die
         entity-move
         entity-set-state
         entity-target-item
         entity-turn-taken?
         entity-use-item
         place-entities
         )

(require (only-in data/collection length)
         "../../color.rkt"
         "../../console.rkt"
         "../../random.rkt"

         "ai-types.rkt"
         "component.rkt"
         "inventory.rkt"
         "item-component.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         )



(define (entity-alive? an-entity)
  (cond [(not (has-component? an-entity 'fighter)) #f]
        [else
         (define fighter (component-get an-entity 'fighter))
         (> (fighter-hp fighter) 0)]))

(define (entity-dead? an-entity)
  (not (entity-alive? an-entity)))

(define (entity-turn-taken? an-entity)
  (ai-type-turn-taken? (component-get an-entity 'ai)))

(define (entity-die an-entity)
  (message-add (format "~s is dead!" (entity-name an-entity))
               #:color color-orange)
  (struct-copy entity
               an-entity
               [char #\%] [color color-dark-red]
               [blocks #f] 
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

;; an-entity is using an item that targets another entity.
(define (entity-target-item an-entity a-target item-idx)
  (define entity-inventory (component-get an-entity 'inventory))
  (cond [(not (< item-idx (length entity-inventory)))
         (values an-entity a-target)]
        [else
         (define new-target
           (item-use (inventory-get entity-inventory item-idx) a-target))
         (define new-player
           (component-update an-entity
                             'inventory
                             (inventory-remove entity-inventory item-idx)))
         (values new-player new-target)]))

(define (entity-use-item an-entity item-idx)
  (define entity-inventory (component-get an-entity 'inventory))
  (cond [(not (< item-idx (length entity-inventory)))
         an-entity]
        [else
         (define new-entity (item-use (inventory-get entity-inventory item-idx) an-entity))
         (if (not new-entity)
             an-entity
             (component-update new-entity
                               'inventory
                               (inventory-remove entity-inventory item-idx)))]))

; NOTE: This could probably also check the fov map so that entities don't get
; placed too close to the player.
; NOTE: Eagerly trying to place an entity might not the best idea, instead it
; should stop after several failed attempts.
(define (place-entities entities a-map)
  (define (place-entity enty placed)
    (define x (random-default-get-int 0 (- MAP-WIDTH 1)))
    (define y (random-default-get-int 0 (- MAP-HEIGHT 1)))
    (cond [(not (tile-is-blocked? x y a-map placed))
           (if (item? enty)
               (struct-copy item enty [x #:parent entity x] [y #:parent entity y])
               (struct-copy entity enty [x x] [y y]))]
          ; Eagerly try to place the entity
          [else (place-entity enty placed)]))
  (for/fold ([placed '()])
            ([e entities])
    (cons (place-entity e placed) placed)))
