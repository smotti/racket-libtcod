#lang racket

(provide make-player
         player-die
         player-handle-input
         player-update)

(require racket/format
         threading

         "../../color.rkt"
         "../../console.rkt"
         "../../fov.rkt"

         "component.rkt"
         "fighter-component.rkt"
         "entity.rkt"
         "inventory.rkt"
         "item-component.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         "utils.rkt"
         )

(define (find-closest-entity player entities within-range fov-map)
  (for/fold ([closest-entity '()]
             [closest-distance (add1 within-range)]
             [entities-not-in-range '()]
             #:result (values closest-entity entities-not-in-range))
            ([ntt entities])
    (cond [(and (has-component? ntt 'fighter)
                (map-is-in-fov fov-map (entity-x ntt) (entity-y ntt)))
           (define distance (distance-to player ntt))
           (if (< distance closest-distance)
               (values ntt distance entities-not-in-range)
               (values closest-entity
                       closest-distance
                       (cons ntt entities-not-in-range)))]
          [else (values closest-entity
                        closest-distance
                        (cons ntt entities-not-in-range))])))

(define (player-die an-entity)
  (message-add "You died!" #:color color-red)
  (struct-copy entity
               an-entity
               [blocks #f]
               [char #\%] [color color-dark-red]))

(define (make-player x y name)
  (make-entity #\@ name
               'ideling 'player
               #:components (hash 'fighter (make-fighter #:hp 30
                                                         #:defense 2
                                                         #:power 5
                                                         #:die player-die)
                                  'inventory (make-inventory))
               #:x x #:y y))

(define (input-key-c->item-idx a-key-char)
  (- (char->integer a-key-char) (char->integer #\a)))

(define (player-attacking? player)
  (eq? 'attacking (entity-state player)))

(define (player-moving? player)
  (eq? 'moving (entity-state player)))

(define (player-picking-up-item? player)
  (eq? 'picking-up-item (entity-state player)))

(define (player-selected-item? keycode)
  (eq? 'CHAR keycode))

(define (player-using-item? player)
  (eq? 'using-item (entity-state player)))

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
  (define player-fighter (component-get player 'fighter))
  (define-values (move-to-x move-to-y) (values (+ player-x dx) (+ player-y dy)))
  (define target-entity (any-fighter-being-attacked? move-to-x move-to-y
                                                     entities))

  (cond [(pick-up-item? input)  ; Player is picking up an item
         (entity-set-state player 'picking-up-item)]

        ; Player opened inventory and is viewing it
        [(and (view-inventory? input) (not (player-viewing-inventory? player)))
         (entity-set-state player 'viewing-inventory)]

        ; Player is viewing the inventory and pressed a item key
        [(and (player-viewing-inventory? player)
              (player-selected-item? (key-vk (game-input-key input))))
         ;(log-debug "Player wants to use item: ~a" (key-c (game-input-key input)))
         (entity-set-state player 'using-item)]

        ; Player is attacking another entity
        [(and target-entity (entity-alive? target-entity))
         ;(log-debug (format "Target: ~v" target-object))
         (define new-player-fighter
           (struct-copy fighter
                        player-fighter
                        [target target-entity]))
         (~> player
             (component-update 'fighter new-player-fighter)
             (entity-set-state 'attacking))]

        ; Player is moving or bumping into a wall
        [else
         (if (tile-is-blocked? move-to-x move-to-y a-map entities)
             (entity-set-state player 'ideling)
             (struct-copy entity player [dx dx] [dy dy] [state 'moving]))]))

(define (player-update player entities items input fov-map)
  (cond [(player-using-item? player)  ; Handle item usage
         (define item-idx (input-key-c->item-idx (key-c (game-input-key input))))
         ;(log-debug "Use item at inventory location: ~v" item-idx)
         (define item (inventory-get (component-get player 'inventory) item-idx))
         (cond [(not (item-needs-target? item))
                (values (entity-use-item player item-idx) entities items)]
               [else
                (define-values (target new-entities)
                  (find-closest-entity player entities (item-range item) fov-map))
                (define-values (new-player new-target)
                  (entity-target-item player target item-idx))
                (values (entity-set-state new-player 'ideling)
                        (if (null? new-target)
                            new-entities
                            (cons new-target new-entities))
                        items)])]

        ; If player is viewing inventory don't update
        [(player-viewing-inventory? player) (values player entities items)]

        ; Handle attack
        [(player-attacking? player)
         (define player-fighter (component-get player 'fighter))
         (define attacked-entity
           (fighter-attack-target player (fighter-target player-fighter)))
         (define new-entities (for/list ([enty entities])
                                (if (equal? enty (fighter-target player-fighter))
                                    attacked-entity
                                    enty)))
         (define new-player-fighter
           (struct-copy fighter player-fighter
                        [target (if (entity-dead? attacked-entity)
                                    #f
                                    attacked-entity)]))
         (define new-player
           (component-update player 'fighter new-player-fighter))
         (values new-player new-entities items)]

        ; Handle moving
        [(player-moving? player)
         (define new-player (entity-move player))
;         (log-debug "Player moves to: ~v - ~v"
;                    (entity-x new-player) (entity-y new-player))
         (values new-player entities items)]

        ; Handle item pick up
        [(player-picking-up-item? player)
         (define-values (an-item new-items)
           (item-pick-up `#(,(entity-x player) ,(entity-y player)) items))
         (define player-inventory (component-get player 'inventory))
         (define new-player
           (~> player
               (component-update 'inventory
                                 (if (not an-item)
                                     player-inventory
                                     (inventory-add player-inventory
                                                    an-item)))
               (entity-set-state 'ideling)))
         (values new-player entities new-items)]

        ; Handle player doing nothing
        [else (values (entity-set-state player 'ideling) entities items)]))
