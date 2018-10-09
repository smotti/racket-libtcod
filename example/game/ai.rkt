#lang racket

(provide ai-handle-state-transition
         ai-update
         make-monster-ai
         (struct-out monster-ai)
         )

(require racket/format
         threading

         "../../color.rkt"
         "../../fov.rkt"

         "ai-types.rkt"
         "components.rkt"
         "entity.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         )


(define (distance-to from to)
  (define dx (- (entity-x to) (entity-x from)))
  (define dy (- (entity-y to) (entity-y from)))
  (sqrt (+ (sqr dx) (sqr dy))))

(define (move-towards an-entity x y state)
  ; vector from obj to target and distance
  (define dx (- x (entity-x an-entity)))
  (define dy (- y (entity-y an-entity)))
  (define distance (sqrt (+ (sqr dx) (sqr dy))))
;  (log-debug "dx: ~v - dy: ~v - distance: ~v" dx dy distance)
  (define ddx (~> dx (/ distance) exact-round))
  (define ddy (~> dy (/ distance) exact-round))

  (if (tile-is-blocked? (+ (entity-x an-entity) ddx)
                     (+ (entity-y an-entity) ddy)
                     (game-state-map state)
                     (game-state-entities state))
      an-entity
      ; normalize vector to length of 1 (preserving direction)
      ; rounding is done to get an integer that is restricted to the map grid
      (entity-move an-entity #:dx ddx #:dy ddy)))

(define (ai-handle-state-transition an-entity state)
  (define an-entity-ai (entity-ai an-entity))
  (cond [(monster-ai? an-entity-ai)
         (monster-ai-handle-state-transition an-entity state)]
        [else an-entity]))

(define (ai-update an-entity state)
  (define an-entity-ai (entity-ai an-entity))
  (cond [(monster-ai? an-entity-ai) (monster-ai-update an-entity state)]
        [else (values an-entity state)]))

(define (monster-attacking? a-monster)
  (eq? 'attacking (entity-state a-monster)))

(define (monster-chasing? a-monster)
  (eq? 'chasing (entity-state a-monster)))

(define (monster-ideling? a-monster)
  (eq? 'ideling (entity-state a-monster)))

(define (monster-in-attack-range? a-monster player)
  (<= (exact-round (distance-to a-monster player)) 1))

(define (monster-not-in-attack-range? a-monster player)
  (>= (exact-round (distance-to a-monster player)) 2))

(define (monster-ai-handle-state-transition a-monster state)
  (define player (game-state-player state))
  (define in-fov? (map-is-in-fov (game-state-fov-map state)
                                 (entity-x a-monster) (entity-y a-monster)))
  (define not-in-fov? (not in-fov?))
;  (log-debug "Distance to player: ~v" (distance-to a-monster player))
  (if (entity-dead? player)
      (if (monster-ideling? a-monster)
          a-monster
          (entity-set-state a-monster 'ideling))
      (cond [(and (monster-ideling? a-monster)
                  in-fov?
                  (monster-not-in-attack-range? a-monster player))
             (entity-set-state a-monster 'chasing)]
            [(and (or (monster-chasing? a-monster) (monster-attacking? a-monster))
                  not-in-fov?)
             (entity-set-state a-monster 'ideling)]
            [(and (or (monster-ideling? a-monster) (monster-chasing? a-monster))
                  in-fov?
                  (monster-in-attack-range? a-monster player))
             (entity-set-state a-monster 'attacking)]
            [(and (monster-attacking? a-monster)
                  in-fov?
                  (monster-not-in-attack-range? a-monster player))
             (entity-set-state a-monster 'chasing)]
            [else a-monster])))

(define (monster-ai-update a-monster state)
  (define player (game-state-player state))
  (cond [(monster-ideling? a-monster)
         (define a-monster-fighter (entity-fighter a-monster))
         (if (not (fighter? a-monster-fighter))
             (values (struct-copy entity a-monster [turn-taken? #t]) state)
             ; TODO: Use lens here
             (values (struct-copy entity a-monster
                                  [turn-taken? #t]
                                  [fighter (struct-copy fighter a-monster-fighter
                                                        [target #f])])
                     state))]
        [(monster-chasing? a-monster)
         (define new-a-monster-fighter
           (struct-copy fighter (entity-fighter a-monster) [target #f]))
         ; TODO: PUll out a procedure to set turn-taken? on an entity, we do the same things several times
         (values (struct-copy entity
                              (move-towards a-monster
                                            (entity-x player)
                                            (entity-y player)
                                            state)
                              [turn-taken? #t])
                 state)]
        [(monster-attacking? a-monster)
;         (log-debug "My position: ~v - ~v"
;                    (entity-x a-monster) (entity-y a-monster))
;         (log-debug "Attack player at: ~v ~v"
;                    (entity-x player) (entity-y player))
         (define a-monster-fighter (entity-fighter a-monster))
         (define new-player-entity (fighter-attack-target a-monster player))
         (values (struct-copy entity a-monster [turn-taken? #t])
                 (struct-copy game-state state [player new-player-entity]))]
        [else (values (struct-copy entity a-monster [turn-taken? #t]) state)]))
