#lang typed/racket

(module ffi-fov racket
  (provide fov-map?
           map-compute-fov
           map-is-in-fov
           map-new
           map-set-properties
           )

  (require ffi/unsafe

           "libtcod.rkt")


  ;;;;
  ;;;; Types
  ;;;;

  (define-cpointer-type _random)

  ;;;;
  ;;;; Field of View
  ;;;;

  (define-cpointer-type _fov-map)

  (define-tcod map-compute-fov
    (_fun _fov-map _int _int _int _bool _int -> _void)
    #:c-id TCOD_map_compute_fov)

  (define-tcod map-is-in-fov
    (_fun _fov-map _int _int -> _bool)
    #:c-id TCOD_map_is_in_fov)

  (define-tcod map-new
    (_fun _int _int -> _fov-map)
    #:c-id TCOD_map_new)

  (define-tcod map-set-properties
    (_fun _fov-map _int _int _bool _bool -> _void)
    #:c-id TCOD_map_set_properties))

(require/typed/provide 'ffi-fov
  [#:opaque FovMap fov-map?]
  [map-compute-fov (-> FovMap Integer Integer Integer Boolean Integer Void)]
  [map-is-in-fov (-> FovMap Integer Integer Boolean)]
  [map-new (-> Integer Integer FovMap)]
  [map-set-properties (-> FovMap Integer Integer Boolean Boolean Void)]
  )
