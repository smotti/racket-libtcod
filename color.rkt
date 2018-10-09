#lang typed/racket

(module ffi-color racket
  (provide _color
          color?
          color-black
          color-dark-blue
          color-dark-red
          color-darker-green
          color-darker-red
          color-desaturated-green
          color-green
          color-light-gray
          color-light-red
          color-orange
          color-red
          color-violet
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

  (define-tcod color-black _color #:c-id TCOD_black)
  (define-tcod color-dark-blue _color #:c-id TCOD_dark_blue)
  (define-tcod color-dark-red _color #:c-id TCOD_dark_red)
  (define-tcod color-darker-green _color #:c-id TCOD_darker_green)
  (define-tcod color-darker-red _color #:c-id TCOD_darker_red)
  (define-tcod color-desaturated-green _color #:c-id TCOD_desaturated_green)
  (define-tcod color-green _color #:c-id TCOD_green)
  (define-tcod color-light-gray _color #:c-id TCOD_light_gray)
  (define-tcod color-light-red _color #:c-id TCOD_light_red)
  (define-tcod color-orange _color #:c-id TCOD_orange)
  (define-tcod color-red _color #:c-id TCOD_red)
  (define-tcod color-violet _color #:c-id TCOD_violet)
  (define-tcod color-white _color #:c-id TCOD_white)
  (define-tcod color-yellow _color #:c-id TCOD_yellow))

(require/typed/provide 'ffi-color
  [#:opaque Color color?]
  [color-black Color]
  [color-dark-blue Color]
  [color-dark-red Color]
  [color-darker-green Color]
  [color-darker-red Color]
  [color-desaturated-green Color]
  [color-green Color]
  [color-light-gray Color]
  [color-light-red Color]
  [color-orange Color]
  [color-red Color]
  [color-violet Color]
  [color-white Color]
  [color-yellow Color]
  [make-color (-> Integer Integer Integer Color)]
  )
