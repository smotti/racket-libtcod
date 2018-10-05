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
         "game-object.rkt"
         "map.rkt"
         "message-log.rkt"
         "types.rkt"
         )


(define (distance-to from to)
  (define dx (- (game-object-x to) (game-object-x from)))
  (define dy (- (game-object-y to) (game-object-y from)))
  (sqrt (+ (sqr dx) (sqr dy))))

(define (move-towards object x y state)
  ; vector from obj to target and distance
  (define dx (- x (game-object-x object)))
  (define dy (- y (game-object-y object)))
  (define distance (sqrt (+ (sqr dx) (sqr dy))))
;  (log-debug "dx: ~v - dy: ~v - distance: ~v" dx dy distance)
  (define ddx (~> dx (/ distance) exact-round))
  (define ddy (~> dy (/ distance) exact-round))

  (if (tile-is-blocked? (+ (game-object-x object) ddx)
                     (+ (game-object-y object) ddy)
                     (game-state-map state)
                     (game-state-objects state))
      object
      ; normalize vector to length of 1 (preserving direction)
      ; rounding is done to get an integer that is restricted to the map grid
      (game-object-move object #:dx ddx #:dy ddy)))

(define (ai-handle-state-transition object state)
  (define object-ai (game-object-ai object))
  (cond [(monster-ai? object-ai)
         (monster-ai-handle-state-transition object state)]
        [else object]))

(define (ai-update object state)
  (define object-ai (game-object-ai object))
  (cond [(monster-ai? object-ai) (monster-ai-update object state)]
        [else (values object state)]))

(define (monster-ai-handle-state-transition object state)
  (define object-ai (game-object-ai object))
  (define object-state (game-object-state object))
  (define player (game-state-player state))
  (define in-fov? (map-is-in-fov (game-state-fov-map state)
                                 (game-object-x object) (game-object-y object)))
  (define not-in-fov? (not in-fov?))
;  (log-debug "Distance to player: ~v" (distance-to object player))
  (if (game-object-dead? player)
      (if (eq? 'ideling object-state)
          object
          (struct-copy game-object object [state 'ideling]))
      (cond [(and (eq? 'ideling object-state)
                  in-fov?
                  (>= (exact-round (distance-to object player)) 2))
             (struct-copy game-object object [state 'chasing])]
            [(and (or (eq? 'chasing object-state) (eq? 'attacking object-state))
                  not-in-fov?)
             (struct-copy game-object object [state 'ideling])]
            [(and (or (eq? 'ideling object-state) (eq? 'chasing object-state))
                  in-fov?
                  (<= (exact-round (distance-to object player)) 1))
             (struct-copy game-object object [state 'attacking] )]
            [(and (eq? 'attacking object-state)
                  in-fov?
                  (>= (exact-round (distance-to object player)) 2))
             (struct-copy game-object object [state 'chasing])]
            [else object])))

(define (monster-ai-update object state)
  (define object-ai (game-object-ai object))
  (define object-state (game-object-state object))
  (define player (game-state-player state))
  (cond [(eq? 'idleing object-state)
         (define object-fighter (game-object-fighter object))
         (if (not (fighter? object-fighter))
             (values object state)
             (values (struct-copy game-object object
                                  [fighter (struct-copy fighter object-fighter
                                                        [target #f])])
                     state))]
        [(eq? 'chasing object-state)
         (define new-object-fighter
           (struct-copy fighter (game-object-fighter object) [target #f]))
         (values (struct-copy game-object
                              (move-towards object
                                            (game-object-x player)
                                            (game-object-y player)
                                            state)
                              [turn-taken? #t])
                 state)]
        [(eq? 'attacking object-state)
;         (log-debug "My position: ~v - ~v"
;                    (game-object-x object) (game-object-y object))
;         (log-debug "Attack player at: ~v ~v"
;                    (game-object-x player) (game-object-y player))
         (define object-fighter (game-object-fighter object))
         (define new-player-object (fighter-attack-target object player))
         (values (struct-copy game-object object [turn-taken? #t])
                 (struct-copy game-state state [player new-player-object]))]
        [else (values (struct-copy game-object object [turn-taken? #t]) state)]))
