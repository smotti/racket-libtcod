#lang typed/racket

(provide make-game-object
         make-position
         make-tile
         GameObject
         Position
         Tile
         (struct-out game-object)
         (struct-out position)
         (struct-out tile)
         )

(require "../../color.rkt")

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
   [blocks? : Boolean])
  #:transparent)
(define-type GameObject game-object)

(: make-game-object (-> Position Char Color Symbol String (#:blocks? Boolean) GameObject))
(define (make-game-object posn char color type name #:blocks? [blocks #f])
  (game-object posn char color type name blocks))

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
