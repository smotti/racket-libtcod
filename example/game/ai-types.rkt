#lang racket

(provide (struct-out ai-type)
         (struct-out monster-ai)
         make-monster-ai
         )


(struct ai-type (type))

(struct monster-ai ai-type (chase? attack?))

(define (make-monster-ai chase? attack?)
  (monster-ai 'monster chase? attack?))
