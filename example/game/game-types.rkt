#lang typed/racket

(provide alive?
         dead?
         die-monster
         die-player
         make-basic-monster
         make-game-object
         make-fighter
         make-position
         make-tile
         message-log
         message-add
         BasicMonster
         GameObject
         GameState
         Fighter
         Position
         Tile
         (struct-out basic-monster)
         (struct-out game-object)
         (struct-out game-state)
         (struct-out fighter)
         (struct-out position)
         (struct-out tile)
         )

(require math/array
         racket/list

         "../../color.rkt"
         "../../fov.rkt"
         "game-constants.rkt"
         )

;;;
;;; Message log
;;;

(: message-log (Boxof (Listof (Pair String Color))))
(define message-log (box '()))

(: message-wrap (-> String Integer (Listof String)))
(define (message-wrap a-message max-width)
  (define message-length (string-length a-message))
  (for/list ([w (range 0 message-length max-width)])
    (define end-index (+ w max-width))
    (substring a-message w (if (> end-index message-length)
                               message-length
                               end-index))))

(: message-add (-> String (#:color Color) Void))
(define (message-add a-message #:color [color color-white])
  (define message-lines (message-wrap a-message MSG-WIDTH))
  (define current-messages (unbox message-log))
  (define new-messages
    (for/fold : (Listof (Pair String Color))
        ([ms current-messages])
        ([m message-lines])
      (if (= (length ms) MSG-HEIGHT)
          (append (drop ms 1) (list (cons m color)))
          (append ms (list (cons m color))))))

  (set-box! message-log new-messages))

;;;
;;; General types
;;;

(struct position ([x : Integer] [y : Integer]) #:transparent)
(define-type Position position)
(define (make-position x y)
  (position x y))

(struct game-object
  ([position : Position]
   [char : Char]
   [color : Color]
   [type : Symbol]
   [name : String]
   [state : Symbol]
   [blocks? : Boolean]
   [fighter : (U Null Fighter)]
   [ai : (U Null BasicMonster)])
  #:mutable
  #:transparent)
(define-type GameObject game-object)

(: make-game-object (-> Position
                        Char
                        Color
                        Symbol
                        String
                        Symbol
                        (#:blocks? Boolean)
                        (#:fighter (U Null Fighter))
                        (#:ai (U Null BasicMonster))
                        GameObject))
(define (make-game-object posn char
                          color
                          type
                          name
                          state
                          #:blocks? [blocks? #f]
                          #:fighter [a-fighter null]
                          #:ai [an-ai null])
  (game-object posn char color type name state blocks? a-fighter an-ai))

(: alive? (-> GameObject Boolean))
(define (alive? obj)
  (eq? (game-object-state obj) 'alive))

(: dead? (-> GameObject Boolean))
(define (dead? obj)
  (eq? (game-object-state obj) 'dead))

(struct game-state ([player : GameObject]
                    [exit : Boolean]
                    [objects : (Listof GameObject)]
                    [map : (Array Tile)]
                    [fov : FovMap]
                    [fov-recompute : Boolean]
                    [mode : Symbol]
                    [action : Symbol]
                    [dead : (Listof GameObject)]
                    ))
(define-type GameState game-state)

;;;
;;; Components (Traits)
;;;

(struct fighter
  ([max-hp : Integer]
   [hp : Integer]
   [defense : Integer]
   [power : Integer]
   [die-proc : (-> GameObject GameObject)])
  #:mutable
  #:transparent)
(define-type Fighter fighter)

(: make-fighter (-> #:hp Integer
                    #:defense Integer
                    #:power Integer
                    (#:die-proc (-> GameObject GameObject))
                    Fighter))
(define (make-fighter #:hp hp
                      #:defense defense
                      #:power power
                      #:die-proc [die-proc (lambda (obj) obj)])
  (fighter hp hp defense power die-proc))

(struct basic-monster ())
(define-type BasicMonster basic-monster)

(define (make-basic-monster)
  (basic-monster))

(: die-player (-> GameObject GameObject))
(define (die-player obj)
  (message-add "You died!" #:color color-red)
  (struct-copy game-object
               obj
               [char #\%] [color color-dark-red]
               [state 'dead]))

(: die-monster (-> GameObject GameObject))
(define (die-monster obj)
  (message-add (format "~s is dead!" (game-object-name obj))
               #:color color-orange)
  (struct-copy game-object
               obj
               [char #\%] [color color-dark-red]
               [blocks? #f] [fighter null] [ai null]
               [name (format "remains of ~s"
                             (game-object-name obj))]
               [state 'dead]))

;;;
;;; Map tile
;;;

(struct tile
  ([blocked : Boolean]
   [block-sight : Boolean]
   [explored : Boolean])
  #:mutable #:transparent)
(define-type Tile tile)
(define (make-tile blocked block-sight explored)
  (tile blocked block-sight explored))
