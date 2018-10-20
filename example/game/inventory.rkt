#lang racket

(provide inventory-add
         inventory-get
         inventory-remove
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

;; Get an item from the inventory (not removing it)
(define (inventory-get inventory item-idx)
  ;(log-debug "Item: ~v" item-idx)
  (ref inventory item-idx))

;; Remove an item from the inventory
;; NOTE: The inventory has to map to the place as displayed meaning 'a' will be
;;       at idx 0 and so forth, but when an item is removed the inventory also needs
;;       to shift the items.
(define (inventory-remove inventory item-idx)
  (define an-item (inventory-get inventory item-idx))
  (extend (pvector) (filter (lambda (i) (not (equal? an-item i))) inventory)))
