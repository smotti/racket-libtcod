#lang typed/racket

(provide make-basic-monster
         make-game-object
         make-fighter
         make-position
         make-tile
         BasicMonster
         GameObject
         Fighter
         Position
         Tile
         (struct-out basic-monster)
         (struct-out game-object)
         (struct-out fighter)
         (struct-out position)
         (struct-out tile)
         )

(require "../../color.rkt")

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
                        (#:blocks? Boolean)
                        (#:fighter (U Null Fighter))
                        (#:ai (U Null BasicMonster))
                        GameObject))
(define (make-game-object posn char
                          color
                          type
                          name
                          #:blocks? [blocks? #f]
                          #:fighter [a-fighter null]
                          #:ai [an-ai null])
  (game-object posn char color type name blocks? a-fighter an-ai))

;;;
;;; Components (Traits)
;;;

(struct fighter
  ([max-hp : Integer]
   [hp : Integer]
   [defense : Integer]
   [power : Integer])
  #:mutable
  #:transparent)
(define-type Fighter fighter)

(: make-fighter (-> #:hp Integer #:defense Integer #:power Integer Fighter))
(define (make-fighter #:hp hp #:defense defense #:power power)
  (fighter hp hp defense power))

(struct basic-monster ())
(define-type BasicMonster basic-monster)

(define (make-basic-monster)
  (basic-monster))

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
