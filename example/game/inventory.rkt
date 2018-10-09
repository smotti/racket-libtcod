#lang racket

(provide inventory-add
         make-inventory
         )

(require data/collection
         data/pvector
         racket/format

         "../../color.rkt"

         "message-log.rkt"
         "types.rkt"
         )


(define MAX-INVENTORY-SIZE 26)

(define (make-inventory) (pvector))

;; Add an item to the inventoy
(define (inventory-add inventory an-item)
  (cond [(< (length inventory) MAX-INVENTORY-SIZE)
         (message-add (format "You picked up a ~a!" (entity-name an-item))
                      #:color color-green)
         (conj inventory an-item)]
        [else
         (message-add (format "Your inventory is full, cannot pick up ~a."
                              (entity-name an-item))
                      #:color color-red)
         inventory]))
