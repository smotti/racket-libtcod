#lang racket

(provide generate-items
         item?
         item-pick-up
         make-item
         )

(require "../../color.rkt"
         "types.rkt")


(define MAX-NUMBER-ITEMS 20)

(define (item? an-entity)
  (eq? 'item (entity-type an-entity)))

;; An entity is picking up an item via item-idx from the given items.
;; item-idx : vector? in form of #(x y)
(define (item-pick-up item-idx items)
  (define an-item (hash-ref items item-idx (lambda () #f)))
  (define new-items (hash-remove items item-idx))
  (values an-item new-items))

(define (make-item char name)
  (make-entity char name 'laying 'item color-violet #:blocks #f))

;; NOTE or TODO: Only makes healing potions for now
(define (generate-items [number MAX-NUMBER-ITEMS])
  (for/list ([i number])
    (make-item #\! "healing potion")))

