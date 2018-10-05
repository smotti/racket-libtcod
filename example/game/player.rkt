#lang racket

(provide make-player
         player-die
         player-handle-input
         player-update)

(require racket/format

         "../../color.rkt"
         "../../console.rkt"

         "components.rkt"
         "entity.rkt"
         "game-state.rkt"
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
               #:fighter (make-fighter #:hp 30
                                       #:defense 2
                                       #:power 5
                                       #:die player-die)))

(define (player-handle-input player input state)
  (define-values (dx dy) (input-move-deltas (key-vk (game-input-key input))))
  (define player-x (entity-x player))
  (define player-y (entity-y player))
  (define player-fighter (entity-fighter player))
  (define-values (move-to-x move-to-y) (values (+ player-x dx) (+ player-y dy)))
  (define entities (game-state-entities state))
  (define target-entity (any-fighter-being-attacked? move-to-x move-to-y
                                                     entities))

  (cond [(and target-entity (entity-alive? target-entity))
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
         (if (tile-is-blocked? move-to-x move-to-y (game-state-map state) entities)
             ; TODO: Just state setting here use new proc
             (struct-copy entity player [state 'ideling])
             (struct-copy entity player
                          [dx dx] [dy dy] [state 'moving]))]))

(define (player-update player state)
  (define player-state (entity-state player))
  ;(log-debug (format "Player state: ~v" player-state))
  (cond [(eq? 'attacking player-state)
         (define player-fighter (entity-fighter player))
         (define attacked-entity
           (fighter-attack-target player (fighter-target player-fighter)))
         (define new-entities (for/list ([enty (game-state-entities state)])
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
         (struct-copy game-state state [player new-player ] [entities new-entities])]
        [(eq? 'moving player-state)
         (define new-player (entity-move player))
;         (log-debug "Player moves to: ~v - ~v"
;                    (entity-x new-player) (entity-y new-player))
         (struct-copy game-state state
                      [player new-player])]
        [else
         ; TODO: Just state setting here use new proc
         (define new-player (struct-copy entity player [state 'ideling]))
         (struct-copy game-state state [player new-player])]))
