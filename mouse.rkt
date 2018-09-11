#lang racket

(provide _mouse
         mouse?)

(require ffi/unsafe
         )

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
                        [lbutton_pressed _bool]
                        [rbutton_pressed _bool]
                        [mbutton_pressed _bool]
                        [wheel-up _bool]
                        [wheel-down _bool]))
