#lang racket

(provide make-player
         player-die
         player-handle-input
         player-update)

(require racket/format

         "../../color.rkt"
         "../../console.rkt"

         "fighter-component.rkt"
         "entity.rkt"
         "inventory.rkt"
         "item-component.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         "utils.rkt"
         )

(define (player-die an-entity)
  (message-add "You died!" #:color color-red)
  (struct-copy entity
               an-entity
               [char #\%] [color color-dark-red]
               [alive? #f]))

(define (make-player x y name)
  (make-entity x y
               #\@ 'player
               name
               'ideling
               0 0
               (make-inventory)
               #:fighter (make-fighter #:hp 30
                                       #:defense 2
                                       #:power 5
                                       #:die player-die)))


(define (player-attacking? player)
  (eq? 'attacking (entity-state player)))

(define (player-moving? player)
  (eq? 'moving (entity-state player)))

(define (player-picking-up-item? player)
  (eq? 'picking-up-item (entity-state player)))

(define (player-viewing-inventory? player)
  (eq? 'viewing-inventory (entity-state player)))

(define (pick-up-item? input)
  (eq? #\g (key-c (game-input-key input))))

(define (view-inventory? input)
  (eq? #\i (key-c (game-input-key input))))

(define (player-handle-input player input entities a-map)
  (define-values (dx dy) (input-move-deltas (key-vk (game-input-key input))))
  (define player-x (entity-x player))
  (define player-y (entity-y player))
  (define player-fighter (entity-fighter player))
  (define-values (move-to-x move-to-y) (values (+ player-x dx) (+ player-y dy)))
  (define target-entity (any-fighter-being-attacked? move-to-x move-to-y
                                                     entities))

  (cond [(pick-up-item? input)  ; Player is picking up an item
         (entity-set-state player 'picking-up-item)]

        ; Player opened inventory and is viewing it
        [(and (view-inventory? input) (not (player-viewing-inventory? player)))
         (entity-set-state player 'viewing-inventory)]

        ; Player is attacking another entity
        [(and target-entity (entity-alive? target-entity))
         ;(log-debug (format "Target: ~v" target-object))
         ; TODO: Maybe we could something here too with lenses, but don't force it if it doesn't help make the code more concise/simpler
         (define new-player-fighter
           (struct-copy fighter
                        player-fighter
                        [target target-entity]))
         (struct-copy entity
                      player
                      [fighter new-player-fighter]
                      [state 'attacking])]
        [else
         (if (tile-is-blocked? move-to-x move-to-y a-map entities)
             (entity-set-state player 'ideling)
             (struct-copy entity player [dx dx] [dy dy] [state 'moving]))]))

(define (player-update player entities items)
  ;(log-debug (format "Player state: ~v" player-state))
  (cond [(player-attacking? player)
         (define player-fighter (entity-fighter player))
         (define attacked-entity
           (fighter-attack-target player (fighter-target player-fighter)))
         (define new-entities (for/list ([enty entities])
                                (if (equal? enty (fighter-target player-fighter))
                                    attacked-entity
                                    enty)))
         ; TODO: Here's a lot of struct copying going on
         (define new-player-fighter
           (struct-copy fighter player-fighter
                        [target (if (entity-dead? attacked-entity)
                                    #f
                                    attacked-entity)]))
         (define new-player
           (struct-copy entity player [fighter new-player-fighter]))
         (values new-player new-entities items)]
        [(player-moving? player)
         (define new-player (entity-move player))
;         (log-debug "Player moves to: ~v - ~v"
;                    (entity-x new-player) (entity-y new-player))
         (values new-player entities items)]
        [(player-picking-up-item? player)
         (define-values (an-item new-items)
           (item-pick-up `#(,(entity-x player) ,(entity-y player)) items))
         (define new-player
           (struct-copy entity player
                        [inventory (if (not an-item)
                                       (entity-inventory player)
                                       (inventory-add (entity-inventory player)
                                                      an-item))]
                        [state 'ideling]))
         (values new-player entities new-items)]
        [else (values (entity-set-state player 'ideling) entities items)]))
