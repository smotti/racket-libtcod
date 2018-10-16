#lang racket

(provide generate-monsters
         monster-die
         )

(require racket/format

         "../../color.rkt"
         "../../random.rkt"

         "ai.rkt"
         "ai-types.rkt"
         "inventory.rkt"
         "message-log.rkt"
         "types.rkt")

(define (generate-monsters number)
  (for/list ([i number])
    (define choice (random-default-get-int 0 100))
    (cond [(< choice 80)
           (make-entity #\o "Orc"
                        'ideling 'monster
                        color-desaturated-green
                        #:components (hash 'fighter
                                           (make-fighter #:hp 10
                                                         #:defense 0
                                                         #:power 3
                                                         #:die monster-die)
                                           'ai (make-monster-ai #t #t)))]
          [else
           (make-entity #\T "Troll"
                        'ideling 'monster
                        color-darker-green
                        #:components (hash 'fighter
                                           (make-fighter #:hp 16
                                                         #:defense 1
                                                         #:power 4
                                                         #:die monster-die)
                                           'ai (make-monster-ai #t #t)))])))

(define (monster-die an-entity)
  (message-add (format "~a is dead!" (entity-name an-entity)))
  (struct-copy entity an-entity
               [char #\%]
               [color color-dark-red]
               [blocks #f]
               [name (format "Remains of ~a" (entity-name an-entity))]))
