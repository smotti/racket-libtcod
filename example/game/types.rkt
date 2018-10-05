#lang typed/racket

(provide (struct-out game-input)
         GameInput
         (struct-out game-object)
         make-game-object
         GameObject
         (struct-out game-state)
         make-game-state
         GameState
         (struct-out fighter)
         make-fighter
         Fighter
         Map
         (struct-out tile)
         Tile
         )

(require (only-in math/array Array)

         "../../color.rkt"
         "../../console.rkt"
         "../../fov.rkt"
         "../../mouse.rkt"
         "../../sys.rkt"

         "ai-types.rkt"
         )

;;;
;;; Game object types
;;;

(struct game-object
  ([x : Integer]
   [y : Integer]
   [dx : Integer]
   [dy : Integer]
   [char : Char]
   [color : Color]
   [type : Symbol]
   [name : String]
   [state : Symbol]
   [blocks : Boolean]
   [fighter : (Option Fighter)]
   [ai : (Option AIType)]
   [turn-taken? : Boolean]
   [alive? : Boolean])
  #:transparent)
(define-type GameObject game-object)

(: make-game-object (->* (Integer Integer
                                  Char
                                  Symbol
                                  String
                                  Symbol)
                         (Integer Integer
                                  Color
                                  Boolean
                                  Boolean
                                  #:blocks Boolean
                                  #:fighter (Option Fighter)
                                  #:ai (Option AIType)
                                  )
                         GameObject))
(define (make-game-object x y
                          char
                          type
                          name
                          state
                          [dx 0] [dy 0]
                          [color color-white]
                          [turn-taken? #f]
                          [alive? #t]
                          #:blocks [blocks #t]
                          #:fighter [a-fighter #f]
                          #:ai [an-ai #f])
  (game-object x y
               dx dy
               char
               color
               type
               name
               state
               blocks
               a-fighter an-ai
               turn-taken? alive?))

;;;
;;; Game state types
;;;

(struct game-input
  ([event : Symbol] [key : Key] [mouse : Mouse]))
(define-type GameInput game-input)

(struct game-state
  ([player : GameObject]
   [objects : (Listof GameObject)]
   [map : Map]
   [fov-map : FovMap]
   [fov-recompute? : Boolean]
   [exit : Boolean]
   [mode : Symbol]
   [action : Symbol]
   [dead : (Listof GameObject)]
   [input : GameInput]
   ))
(define-type GameState game-state)

(: make-game-state (->* (GameObject (Listof GameObject)  ; Mandatory fields
                         Map FovMap)
                        (Boolean Boolean  ; Optional fields
                                 Symbol Symbol
                                 (Listof GameObject)
                                 GameInput)
                        GameState))
(define (make-game-state player objects
                         map fov-map
                         [fov-recompute #t] [exit #f]
                         [mode 'playing] [action 'no-turn]
                         [dead '()]
                         [input (game-input 'KEY_PRESS_MOUSE_MOVE
                                            (make-key-default)
                                            (make-mouse-default))])
  (game-state player objects map fov-map fov-recompute exit mode action dead input))

;;;
;;; Component types
;;;

(struct fighter
  ([max-hp : Integer]
   [hp : Integer]
   [defense : Integer]
   [power : Integer]
   [target : (Option GameObject)]
   [die : (-> GameObject GameObject)])
  #:transparent)
(define-type Fighter fighter)

(: make-fighter (-> #:hp Integer
                    #:defense Integer
                    #:power Integer
                    (#:target GameObject)
                    (#:die (-> GameObject GameObject))
                    Fighter))
(define (make-fighter #:hp hp
                      #:defense defense
                      #:power power
                      #:target [target #f]
                      #:die [die (lambda ([obj : GameObject]) obj)])
  (fighter hp hp defense power target die))

;;;
;;; Map types
;;;

(struct tile
  ([blocked : Boolean]
   [block-sight : Boolean]
   [explored : Boolean])
  #:mutable #:transparent)
(define-type Tile tile)

(define-type Map (Array Tile))
