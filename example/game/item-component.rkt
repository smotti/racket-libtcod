#lang racket

(provide generate-items
         (struct-out item)
         item?
         item-pick-up
         item-use
         make-item
         )

(require "../../color.rkt"

         "component.rkt"
         "fighter-component.rkt"
         "message-log.rkt"
         "types.rkt")


(define MAX-NUMBER-ITEMS 20)

;; use-proc takes an entity that wants to use the item and either returns an
;; updated entity or #f if the entity didn't need to be updated.
(struct item entity (use-proc))

;(define (item? an-entity)
;  (eq? 'item (entity-type an-entity)))

;; An entity is picking up an item via item-idx from the given items.
;; item-idx : vector? in form of #(x y)
(define (item-pick-up item-idx items)
  (define an-item (hash-ref items item-idx (lambda () #f)))
  (define new-items (hash-remove items item-idx))
  (values an-item new-items))

(define (make-item char name use-proc [color color-violet])
  (item #f char color '() 0 0 name 'laying 'item 0 0 use-proc))

;; Apply an item to entity
(define (item-use item entity)
  ((item-use-proc item) entity))

;; NOTE or TODO: Only makes healing potions for now
(define (generate-items [number MAX-NUMBER-ITEMS])
  (for/list ([i number])
    (make-item #\! "healing potion" heal)))


;;; Concret item use procedures

(define (heal an-entity [amount 4])
  (define entity-fighter (component-get an-entity 'fighter))
  (cond [(not (= (fighter-hp entity-fighter) (fighter-max-hp entity-fighter)))
         (message-add "Your wounds start to feel better!"
                      #:color color-light-violet)
         (define new-hp (+ (fighter-hp entity-fighter) amount))
         (define max-hp (fighter-max-hp entity-fighter))
         (component-update an-entity
                           'fighter
                           (struct-copy fighter entity-fighter
                                        [hp (if (> new-hp max-hp) max-hp new-hp)]))]
        [else
         (message-add "You're already at full health." #:color color-red)
         #f]))

