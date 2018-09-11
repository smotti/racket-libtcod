#lang typed/racket

(module ffi-color racket
  (provide _color
          color?
          color-dark-blue
          color-darker-green
          color-desaturated-green
          color-white
          color-yellow
          make-color
          )

  (require ffi/unsafe

           "libtcod.rkt")

  ;;;
  ;;; Types
  ;;;

  (define-cstruct _color ([r _uint8]
                          [g _uint8]
                          [b _uint8]))

  ;;;
  ;;; Colors
  ;;;

  (define-tcod color-dark-blue _color #:c-id TCOD_dark_blue)
  (define-tcod color-darker-green _color #:c-id TCOD_darker_green)
  (define-tcod color-desaturated-green _color #:c-id TCOD_desaturated_green)
  ;(define color-darker-green (make-color 0 127 0))
  ;(define color-desaturated-green (make-color 63 127 63))
  (define-tcod color-white _color #:c-id TCOD_white)
  (define-tcod color-yellow _color #:c-id TCOD_yellow))

(require/typed/provide 'ffi-color
  [#:opaque Color color?]
  [color-dark-blue Color]
  [color-darker-green Color]
  [color-desaturated-green Color]
  [color-white Color]
  [color-yellow Color]
  [make-color (-> Integer Integer Integer Color)]
  )
