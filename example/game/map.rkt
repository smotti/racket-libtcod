#lang typed/racket

(require math/array

         "../../color.rkt"
         "game-types.rkt")

(require/typed "../../random.rkt"
  [random-default-get-int (-> Integer Integer Integer)])

(provide is-blocked?
         make-map
         MAP-WIDTH
         MAP-HEIGHT
         )

;;;
;;; Constants
;;;

;; for map
(define MAP-WIDTH 80)
(define MAP-HEIGHT 45)

;; for rooms
(define ROOM-MAX-SIZE 10)
(define ROOM-MIN-SIZE 6)
(define MAX-ROOMS 30)
(define MAX-ROOM-MONSTERS 3)

;;;
;;; Rectangle
;;;

(struct rectangle ([x1 : Integer] [y1 : Integer]
                                  [x2 : Integer] [y2 : Integer]
                                  [w : Integer] [h : Integer]))
(define-type Rectangle rectangle)
(define-type RectangleRoom rectangle)

(: make-rectangle (-> Integer Integer Integer Integer Rectangle))
(define (make-rectangle x y w h)
  (rectangle x y
             (+ x w) (+ y h)
             w h))

(: center (-> RectangleRoom (Values Integer Integer)))
(define (center room)
  (let ([x1 (rectangle-x1 room)]
        [x2 (rectangle-x2 room)]
        [y1 (rectangle-y1 room)]
        [y2 (rectangle-y2 room)])
    (values (floor (/ (+ x1 x2) 2))
            (floor (/ (+ y1 y2) 2)))))

(: intersect (-> RectangleRoom RectangleRoom Boolean))
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

(: is-blocked? (-> Integer Integer (Array Tile) (Listof GameObject) Boolean))
(define (is-blocked? x y a-map objs)
  (define a-tile (array-ref a-map `#(,y ,x)))

  (cond
    [(tile-blocked a-tile) #t]
    [else (for/or ([o objs])
            (define o-posn (game-object-position o))
            (and (game-object-blocks? o)
                 (= (position-x o-posn) x)
                 (= (position-y o-posn) y)))]))

;;;
;;; Rooms
;;;

(: create-room (-> Rectangle (Array Tile) Void))
(define (create-room shape a-map)
  (for ([x (in-range (+ (rectangle-x1 shape) 1)
                     (rectangle-x2 shape))])
    (for ([y (in-range (+ (rectangle-y1 shape) 1)
                       (rectangle-y2 shape))])
      (: t Tile)
      (define t (array-ref a-map `#(,y ,x)))
      (set-tile-blocked! t #f)
      (set-tile-block-sight! t #f))))

(: create-h-tunnel (-> Integer Integer Integer (Array Tile) Void))
(define (create-h-tunnel x1 x2 y a-map)
  (for ([x (in-range (min x1 x2) (+ (max x1 x2) 1))])
    (: t Tile)
    (define t (array-ref a-map `#(,y ,x)))
    (set-tile-blocked! t #f)
    (set-tile-block-sight! t #f)))

(: create-v-tunnel (-> Integer Integer Integer (Array Tile) Void))
(define (create-v-tunnel y1 y2 x a-map)
  (for ([y (in-range (min y1 y2) (+ (max y1 y2) 1))])
    (: t Tile)
    (define t (array-ref a-map `#(,y ,x)))
    (set-tile-blocked! t #f)
    (set-tile-block-sight! t #f)))

(: place-objects (-> RectangleRoom (Array Tile) (Listof GameObject)))
(define (place-objects room a-map)
  (define num-monsters (random-default-get-int 0 MAX-ROOM-MONSTERS))

  (: accumulate-objects (-> (Listof GameObject) Integer (Listof GameObject)))
  (define (accumulate-objects objs counter)
    (define x (random-default-get-int (rectangle-x1 room) (rectangle-x2 room)))
    (define y (random-default-get-int (rectangle-y1 room) (rectangle-y2 room)))

    (cond
      [(= counter num-monsters) objs]
      [(is-blocked? x y a-map objs)
       (accumulate-objects objs counter)]
      [else
       (define choice (random-default-get-int 0 100))
       (cond
         [(< choice 80)
          (define new-obj (make-game-object (make-position x y)
                                           #\o 
                                           color-desaturated-green
                                           'monster
                                           "Orc"
                                           #:blocks? #t))
          (accumulate-objects (cons new-obj objs) (+ 1 counter))]
         [else
          (define new-obj (make-game-object (make-position x y)
                                            #\T
                                            color-darker-green
                                            'monster
                                            "Troll"
                                            #:blocks? #t))
          (accumulate-objects (cons new-obj objs) (+ 1 counter))])]))

  (accumulate-objects '() 0))

;;;
;;; Map
;;;

(: make-map (-> (Values (Array Tile) (Listof GameObject) Position)))
(define (make-map)
  (define a-map (build-array `#(,MAP-HEIGHT ,MAP-WIDTH)
                           (lambda (is)
                             (make-tile #t #t #f))))

  (define (make-new-room)
    (define w (random-default-get-int ROOM-MIN-SIZE ROOM-MAX-SIZE))
    (define h (random-default-get-int ROOM-MIN-SIZE ROOM-MAX-SIZE))
    (define x (random-default-get-int 0 (- MAP-WIDTH w 1)))
    (define y (random-default-get-int 0 (- MAP-HEIGHT h 1)))
    (make-rectangle x y w h))

  (define first-room (make-new-room))
  (define player-start-position
    (let-values ([(x y) (center first-room)])
      (position x y)))

  (: does-intersect? (-> (Listof RectangleRoom) RectangleRoom Boolean))
  (define (does-intersect? rooms new-room)
    (for/or : Boolean ([room rooms])
      (intersect new-room room)))

  (: make-rooms (-> (Listof RectangleRoom) (Listof GameObject) RectangleRoom Integer
                    (Values (Listof RectangleRoom) (Listof GameObject))))
  (define (make-rooms rooms objs new-room no-of-rooms)
    (cond
      [(= MAX-ROOMS no-of-rooms) (values rooms objs)]
      [(does-intersect? rooms new-room)
       (log-debug "New room intersects with existing room!")
       (make-rooms rooms objs (make-new-room) no-of-rooms)]
      [else
       (log-debug "Create new room")
       (create-room new-room a-map)
       (define-values (new-x new-y) (center new-room))

       (cond
         [(= 0 no-of-rooms)
          ; Put player at center of first room
          (log-debug "Putting player at the center of first room")
          ]
         [else
          (log-debug "Connect new room to existing rooms")
          (define-values (prev-x prev-y) (center (first rooms)))
          (cond
            [(= 1 (random-default-get-int 0 1))
             (create-h-tunnel prev-x new-x prev-y a-map)
             (create-v-tunnel prev-y new-y new-x a-map)]
            [else (create-v-tunnel prev-y new-y prev-x a-map)
                  (create-h-tunnel prev-x new-x new-y a-map)])])
       (make-rooms (cons new-room rooms)
                   (append (place-objects new-room a-map) objs)
                   (make-new-room)
                   (+ 1 no-of-rooms))]))

  (define-values (rooms objs) (make-rooms '() '() first-room 0))
  (log-debug "Created map")

  (values a-map objs player-start-position))
