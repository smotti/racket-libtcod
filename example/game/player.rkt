#lang racket

(provide make-player
         player-die
         player-handle-input
         player-update)

(require racket/format

         "../../color.rkt"
         "../../console.rkt"

         "components.rkt"
         "game-object.rkt"
         "game-state.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         "utils.rkt"
         )

(define (player-die obj)
  (message-add "You died!" #:color color-red)
  (struct-copy game-object
               obj
               [char #\%] [color color-dark-red]
               [alive? #f]))

(define (make-player x y name)
  (make-game-object x y
                    #\@ 'player
                    name
                    'ideling
                    #:fighter (make-fighter #:hp 30
                                            #:defense 2
                                            #:power 5
                                            #:die player-die)))

(define (player-handle-input player input state)
  (define-values (dx dy) (input-move-deltas (key-vk (game-input-key input))))
  (define player-x (game-object-x player))
  (define player-y (game-object-y player))
  (define player-fighter (game-object-fighter player))
  (define-values (move-to-x move-to-y) (values (+ player-x dx) (+ player-y dy)))
  (define objects (game-state-objects state))
  (define target-object (any-fighter-being-attacked? move-to-x move-to-y
                                                     objects))

  (cond [(and target-object (game-object-alive? target-object))
         ;(log-debug (format "Target: ~v" target-object))
         (define new-player-fighter
           (struct-copy fighter
                        player-fighter
                        [target target-object]))
         (struct-copy game-object
                      player
                      [fighter new-player-fighter]
                      [state 'attacking])]
        [else
         (if (tile-is-blocked? move-to-x move-to-y (game-state-map state) objects)
             (struct-copy game-object player [state 'ideling])
             (struct-copy game-object player
                          [dx dx] [dy dy] [state 'moving]))]))

(define (player-update player state)
  (define player-state (game-object-state player))
  ;(log-debug (format "Player state: ~v" player-state))
  (cond [(eq? 'attacking player-state)
         (define player-fighter (game-object-fighter player))
         (define attacked-object
           (fighter-attack-target player (fighter-target player-fighter)))
         (define new-objects (for/list ([obj (game-state-objects state)])
                               (if (equal? obj (fighter-target player-fighter))
                                   attacked-object
                                   obj)))
         (define new-player-fighter
           (struct-copy fighter player-fighter
                        [target (if (game-object-dead? attacked-object)
                                    #f
                                    attacked-object)]))
         (define new-player
           (struct-copy game-object player [fighter new-player-fighter]))
         (struct-copy game-state state [player new-player ][objects new-objects])]
        [(eq? 'moving player-state)
         (define new-player (game-object-move player))
;         (log-debug "Player moves to: ~v - ~v"
;                    (game-object-x new-player) (game-object-y new-player))
         (struct-copy game-state state
                      [player new-player])]
        [else
         (define new-player (struct-copy game-object player [state 'ideling]))
         (struct-copy game-state state [player new-player])]))
