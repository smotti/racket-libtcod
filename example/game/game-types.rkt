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

         "../../color.rkt"
         "../../fov.rkt"
         )

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
  (log-debug "You died!")
  (struct-copy game-object
               obj
               [char #\%] [color color-dark-red]
               [state 'dead]))

(: die-monster (-> GameObject GameObject))
(define (die-monster obj)
  (log-debug "~s is dead!" (game-object-name obj))
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
