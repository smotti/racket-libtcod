#lang racket

(provide ai-handle-state-transition
         ai-reset-turn-taken
         ai-update
         make-monster-ai
         (struct-out monster-ai)
         )

(require racket/format
         threading

         "../../color.rkt"
         "../../fov.rkt"

         "ai-types.rkt"
         "component.rkt"
         "fighter-component.rkt"
         "entity.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         )

(define (distance-to from to)
  (define dx (- (entity-x to) (entity-x from)))
  (define dy (- (entity-y to) (entity-y from)))
  (sqrt (+ (sqr dx) (sqr dy))))

(define (move-towards an-entity x y a-map entities)
  ; vector from obj to target and distance
  (define dx (- x (entity-x an-entity)))
  (define dy (- y (entity-y an-entity)))
  (define distance (sqrt (+ (sqr dx) (sqr dy))))
;  (log-debug "dx: ~v - dy: ~v - distance: ~v" dx dy distance)
  (define ddx (~> dx (/ distance) exact-round))
  (define ddy (~> dy (/ distance) exact-round))

  (if (tile-is-blocked? (+ (entity-x an-entity) ddx)
                        (+ (entity-y an-entity) ddy)
                        a-map
                        entities)
      an-entity
      ; normalize vector to length of 1 (preserving direction)
      ; rounding is done to get an integer that is restricted to the map grid
      (entity-move an-entity #:dx ddx #:dy ddy)))

(define (ai-handle-state-transition an-entity player fov-map)
  (define an-entity-ai (component-get an-entity 'ai))
  (cond [(ai-type-monster? an-entity-ai)
         (monster-ai-handle-state-transition an-entity player fov-map)]
        [else an-entity]))

(define (ai-update an-entity player a-map entities)
  (define an-entity-ai (component-get an-entity 'ai))
  (cond [(ai-type-monster? an-entity-ai)
         (monster-ai-update an-entity player a-map entities)]
        [else (values an-entity player)]))

(define (ai-take-turn an-ai)
  (struct-copy ai-type an-ai [turn-taken? #t]))

(define (ai-reset-turn-taken an-ai)
  (struct-copy ai-type an-ai [turn-taken? #f]))

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

(define (monster-ai-handle-state-transition a-monster player fov-map)
  (define in-fov? (map-is-in-fov fov-map
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

(define (monster-ai-update a-monster player a-map entities)
  (define a-monster-fighter (component-get a-monster 'fighter))
  (define ai (ai-take-turn (component-get a-monster 'ai)))
  (cond [(monster-ideling? a-monster)
         (if (not a-monster-fighter)
             (values (component-update a-monster 'ai ai)
                     player)
             (values (~> a-monster
                         (component-update 'ai ai)
                         (component-update 'fighter
                                                  (struct-copy fighter
                                                               a-monster-fighter
                                                               [target #f])))
                     player))]
        [(monster-chasing? a-monster)
         (define new-a-monster-fighter
           (struct-copy fighter a-monster-fighter [target #f]))
         (values (~> a-monster
                     (move-towards (entity-x player)
                                   (entity-y player)
                                   a-map
                                   entities)
                     (component-update 'ai ai)
                     (component-update 'fighter new-a-monster-fighter))
                 player)]
        [(monster-attacking? a-monster)
;         (log-debug "My position: ~v - ~v"
;                    (entity-x a-monster) (entity-y a-monster))
;         (log-debug "Attack player at: ~v ~v"
;                    (entity-x player) (entity-y player))
         (define new-player-entity (fighter-attack-target a-monster player))
         (values (component-update a-monster 'ai ai) new-player-entity)]
        [else (values (component-update a-monster 'ai ai) player)]))
