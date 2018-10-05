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

(define (monster-ai-handle-state-transition an-entity state)
  (define an-entity-ai (entity-ai an-entity))
  (define an-entity-state (entity-state an-entity))
  (define player (game-state-player state))
  (define in-fov? (map-is-in-fov (game-state-fov-map state)
                                 (entity-x an-entity) (entity-y an-entity)))
  (define not-in-fov? (not in-fov?))
;  (log-debug "Distance to player: ~v" (distance-to an-entity player))
  ; TODO: Pull out a procedure to set a new state for a given an-entity
  (if (entity-dead? player)
      (if (eq? 'ideling an-entity-state)
          an-entity
          (struct-copy entity an-entity [state 'ideling]))
      (cond [(and (eq? 'ideling an-entity-state)
                  in-fov?
                  (>= (exact-round (distance-to an-entity player)) 2))
             (struct-copy entity an-entity [state 'chasing])]
            [(and (or (eq? 'chasing an-entity-state) (eq? 'attacking an-entity-state))
                  not-in-fov?)
             (struct-copy entity an-entity [state 'ideling])]
            [(and (or (eq? 'ideling an-entity-state) (eq? 'chasing an-entity-state))
                  in-fov?
                  (<= (exact-round (distance-to an-entity player)) 1))
             (struct-copy entity an-entity [state 'attacking] )]
            [(and (eq? 'attacking an-entity-state)
                  in-fov?
                  (>= (exact-round (distance-to an-entity player)) 2))
             (struct-copy entity an-entity [state 'chasing])]
            [else an-entity])))

(define (monster-ai-update an-entity state)
  (define an-entity-ai (entity-ai an-entity))
  (define an-entity-state (entity-state an-entity))
  (define player (game-state-player state))
  (cond [(eq? 'idleing an-entity-state)
         (define an-entity-fighter (entity-fighter an-entity))
         (if (not (fighter? an-entity-fighter))
             (values an-entity state)
             ; TODO: Use lens here
             (values (struct-copy entity an-entity
                                  [fighter (struct-copy fighter an-entity-fighter
                                                        [target #f])])
                     state))]
        [(eq? 'chasing an-entity-state)
         (define new-an-entity-fighter
           (struct-copy fighter (entity-fighter an-entity) [target #f]))
         ; TODO: PUll out a procedure to set turn-taken? on an entity, we do the same things several times
         (values (struct-copy entity
                              (move-towards an-entity
                                            (entity-x player)
                                            (entity-y player)
                                            state)
                              [turn-taken? #t])
                 state)]
        [(eq? 'attacking an-entity-state)
;         (log-debug "My position: ~v - ~v"
;                    (entity-x an-entity) (entity-y an-entity))
;         (log-debug "Attack player at: ~v ~v"
;                    (entity-x player) (entity-y player))
         (define an-entity-fighter (entity-fighter an-entity))
         (define new-player-entity (fighter-attack-target an-entity player))
         (values (struct-copy entity an-entity [turn-taken? #t])
                 (struct-copy game-state state [player new-player-entity]))]
        [else (values (struct-copy entity an-entity [turn-taken? #t]) state)]))
