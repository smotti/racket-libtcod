#lang typed/racket

(module ffi-mouse racket
  (provide make-mouse-default
           _mouse
           mouse?
           mouse-x
           mouse-y
           mouse-dx
           mouse-dy
           mouse-cx
           mouse-cy
           mouse-dcx
           mouse-dcy
           mouse-lbutton
           mouse-rbutton
           mouse-mbutton
           mouse-lbutton-pressed
           mouse-rbutton-pressed
           mouse-mbutton-pressed
           mouse-wheel-up
           mouse-wheel-down)

  (require ffi/unsafe)

  ;;;
  ;;; Types
  ;;;

  (define-cstruct _mouse ([x _int]
                          [y _int]
                          [dx _int]
                          [dy _int]
                          [cx _int]
                          [cy _int]
                          [dcx _int]
                          [dcy _int]
                          [lbutton _bool]
                          [rbutton _bool]
                          [mbutton _bool]
                          [lbutton-pressed _bool]
                          [rbutton-pressed _bool]
                          [mbutton-pressed _bool]
                          [wheel-up _bool]
                          [wheel-down _bool]))
  (define (make-mouse-default)
    (make-mouse 0 0 0 0 0 0 0 0 #f #f #f #f #f #f #f #f)))

(require/typed/provide 'ffi-mouse
  [#:opaque Mouse mouse?]
  [make-mouse-default (-> Mouse)]
  [mouse-x (-> Mouse Integer)]
  [mouse-y (-> Mouse Integer)]
  [mouse-dx (-> Mouse Integer)]
  [mouse-dy (-> Mouse Integer)]
  [mouse-cx (-> Mouse Integer)]
  [mouse-cy (-> Mouse Integer)]
  [mouse-dcx (-> Mouse Integer)]
  [mouse-dcy (-> Mouse Integer)]
  [mouse-lbutton (-> Mouse Boolean)]
  [mouse-rbutton (-> Mouse Boolean)]
  [mouse-mbutton (-> Mouse Boolean)]
  [mouse-lbutton-pressed (-> Mouse Boolean)]
  [mouse-rbutton-pressed (-> Mouse Boolean)]
  [mouse-mbutton-pressed (-> Mouse Boolean)]
  [mouse-wheel-up (-> Mouse Boolean)]
  [mouse-wheel-down (-> Mouse Boolean)]
  )
