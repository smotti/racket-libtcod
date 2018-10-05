#lang typed/racket

(provide (struct-out ai-type)
         AIType
         (struct-out monster-ai)
         MonsterAI
         make-monster-ai
         )


(struct ai-type ([type : Symbol]))
(define-type AIType ai-type)

(struct monster-ai ai-type ([chase? : Boolean] [attack? : Boolean]))
(define-type MonsterAI monster-ai)

(: make-monster-ai (-> Boolean Boolean MonsterAI))
(define (make-monster-ai chase? attack?)
  (monster-ai 'monster chase? attack?))

;(: monster-ai? (-> AIType Boolean))
;(define (monster-ai? ai)
;  (eq? 'monster (ai-type-type ai)))
