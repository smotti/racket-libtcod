#lang racket

(provide (struct-out game-input)
         (struct-out entity)
         make-entity
         (struct-out game-state)
         make-game-state
         (struct-out fighter)
         make-fighter
         (struct-out tile)
         )

(require "../../color.rkt"
         "../../console.rkt"
         "../../fov.rkt"
         "../../mouse.rkt"
         "../../sys.rkt"

         "ai-types.rkt"
         )

;;;
;;; Game object types
;;;

(struct entity
  (x y dx dy
   char color
   type
   name
   state
   blocks
   fighter
   ai
   turn-taken?
   alive?)
  #:transparent)

(define (make-entity x y
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
  (entity x y
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

(struct game-input (event key mouse))

(struct game-state
  (player entities map fov-map fov-recompute? exit mode action dead input))

(define (make-game-state player entities
                         map fov-map
                         [fov-recompute #t] [exit #f]
                         [mode 'playing] [action 'no-turn]
                         [dead '()]
                         [input (game-input 'KEY_PRESS_MOUSE_MOVE
                                            (make-key-default)
                                            (make-mouse-default))])
  (game-state player entities map fov-map fov-recompute exit mode action dead input))

;;;
;;; Component types
;;;

(struct fighter
  (max-hp hp defense power target die)
  #:transparent)

(define (make-fighter #:hp hp
                      #:defense defense
                      #:power power
                      #:target [target #f]
                      #:die [die (lambda (obj) obj)])
  (fighter hp hp defense power target die))

;;;
;;; Map types
;;;

(struct tile (blocked? block-sight explored?)
  #:mutable #:transparent)
