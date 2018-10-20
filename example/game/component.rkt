#lang racket

(provide component-get
         has-component?
         component-update
         )

(require "types.rkt")


(define (component-get an-entity component)
  (hash-ref (entity-components an-entity) component (lambda () '())))

(define (component-update an-entity component new-component-value)
  (define components (entity-components an-entity))
  (struct-copy entity an-entity
               [components (hash-update components
                                        component
                                        (lambda (_) new-component-value)
                                        (lambda () components))]))

(define (has-component? an-entity component)
  (hash-ref (entity-components an-entity) component (lambda () #f)))
