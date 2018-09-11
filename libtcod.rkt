#lang racket

(provide define-tcod
         )

(require ffi/unsafe
         ffi/unsafe/define)


(define-ffi-definer define-tcod (ffi-lib "libtcod"))
