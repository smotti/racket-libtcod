#lang racket

(provide make-fov-map
         make-map
         map-ref
         MAP-WIDTH
         MAP-HEIGHT
         tile-is-blocked?
         tile-wall?
         )

(require threading

         "../../color.rkt"
         "../../fov.rkt"
         "../../random.rkt"

         "ai-types.rkt"
         "types.rkt")
;;;
;;; Map tile
;;;

(define (make-tile blocked? block-sight explored?)
  (tile blocked? block-sight explored?))

;;;
;;; Constants
;;;

;; for map
(define MAP-WIDTH 80)
(define MAP-HEIGHT 43)

;; for rooms
(define ROOM-MAX-SIZE 10)
(define ROOM-MIN-SIZE 6)
(define MAX-ROOMS 30)

;;;
;;; Rectangle
;;;

(struct rectangle (x1 y1 x2 y2 w h))

(define (make-rectangle x y w h)
  (rectangle x y (+ x w) (+ y h) w h))

(define (center room)
  (let ([x1 (rectangle-x1 room)]
        [x2 (rectangle-x2 room)]
        [y1 (rectangle-y1 room)]
        [y2 (rectangle-y2 room)])
    (values (floor (/ (+ x1 x2) 2))
            (floor (/ (+ y1 y2) 2)))))

(define (intersect r1 r2)
  (let ([r1-x1 (rectangle-x1 r1)]
        [r1-x2 (rectangle-x2 r1)]
        [r1-y1 (rectangle-y1 r1)]
        [r1-y2 (rectangle-y2 r1)]
        [r2-x1 (rectangle-x1 r2)]
        [r2-x2 (rectangle-x2 r2)]
        [r2-y1 (rectangle-y1 r2)]
        [r2-y2 (rectangle-y2 r2)])
    (and (<= r1-x1 r2-x2) (>= r1-x2 r2-x1)
         (<= r1-y2 r2-y2) (>= r1-y2 r2-y1))))

;;;
;;; Helpers
;;;

(define (map-ref a-map x y)
  (~> a-map
      (vector-ref y)
      (vector-ref x)))

(define (tile-is-blocked? x y a-map entities)
  (define a-tile (map-ref a-map x y))

  (cond
    [(tile-blocked? a-tile) #t]
    [else (for/or ([e entities])
            (and (entity-blocks e)
                 (= (entity-x e) x)
                 (= (entity-y e) y)))]))

(define (tile-wall? a-tile)
  (tile-block-sight a-tile))

;;;
;;; Rooms
;;;

(define (create-room shape a-map)
  (for ([x (in-range (add1 (rectangle-x1 shape))
                     (rectangle-x2 shape))])
    (for ([y (in-range (add1 (rectangle-y1 shape))
                       (rectangle-y2 shape))])
      (define t (map-ref a-map x y))
      (set-tile-blocked?! t #f)
      (set-tile-block-sight! t #f))))

(define (create-h-tunnel x1 x2 y a-map)
  (for ([x (in-range (min x1 x2) (add1 (max x1 x2)))])
    (define t (map-ref a-map x y))
    (set-tile-blocked?! t #f)
    (set-tile-block-sight! t #f)))

(define (create-v-tunnel y1 y2 x a-map)
  (for ([y (in-range (min y1 y2) (add1 (max y1 y2)))])
    (define t (map-ref a-map x y))
    (set-tile-blocked?! t #f)
    (set-tile-block-sight! t #f)))

;;;
;;; Map
;;;

(define (make-map)
  (define a-map
    (vector->immutable-vector
     (for/vector ([y MAP-HEIGHT])
       (vector->immutable-vector
        (for/vector ([x MAP-WIDTH])
          (make-tile #t #t #f))))))

  (define (make-new-room)
    (define w (random-default-get-int ROOM-MIN-SIZE ROOM-MAX-SIZE))
    (define h (random-default-get-int ROOM-MIN-SIZE ROOM-MAX-SIZE))
    (define x (random-default-get-int 0 (- MAP-WIDTH w 1)))
    (define y (random-default-get-int 0 (- MAP-HEIGHT h 1)))
    (make-rectangle x y w h))

  (define first-room (make-new-room))
  (define player-start-position
    (let-values ([(x y) (center first-room)])
      (cons x y)))

  (define (does-intersect? rooms new-room)
    (for/or ([room rooms]) (intersect new-room room)))

  (define (make-rooms rooms new-room no-of-rooms)
    (cond
      [(= MAX-ROOMS no-of-rooms) rooms]
      [(does-intersect? rooms new-room)
;       (log-debug "New room intersects with existing room!")
       (make-rooms rooms (make-new-room) no-of-rooms)]
      [else
;       (log-debug "Create new room")
       (create-room new-room a-map)

       (define-values (new-x new-y) (center new-room))
;       (log-debug "Connect new room to existing rooms")

       (when (> no-of-rooms 0)
         (define-values (prev-x prev-y) (center (first rooms)))
         (cond
           [(= 1 (random-default-get-int 0 1))
            (create-h-tunnel prev-x new-x prev-y a-map)
            (create-v-tunnel prev-y new-y new-x a-map)]
           [else (create-v-tunnel prev-y new-y prev-x a-map)
                 (create-h-tunnel prev-x new-x new-y a-map)]))
       (make-rooms (cons new-room rooms)
                   (make-new-room)
                   (add1 no-of-rooms))]))

  (make-rooms '() first-room 0)
  (log-debug "Created map")

  (values a-map player-start-position))

(define (make-fov-map w h a-map)
  (define fov-map (map-new MAP-WIDTH MAP-HEIGHT))
  (for ([y MAP-HEIGHT])
    (for ([x MAP-WIDTH])
      (let ([t (map-ref a-map x y)])
        (map-set-properties fov-map
                            x y
                            (not (tile-block-sight t)) (not (tile-blocked? t))))))
  fov-map)
