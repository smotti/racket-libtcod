#lang racket

(provide (struct-out ai-type)
         ai-type-monster?
         (struct-out monster-ai)
         make-monster-ai
         )


(struct ai-type (type turn-taken?) #:transparent)

(define (ai-type-monster? an-ai)
  (eq? 'monster (ai-type-type an-ai)))

(struct monster-ai ai-type (chase? attack?))

(define (make-monster-ai chase? attack?)
  (monster-ai 'monster #f chase? attack?))
