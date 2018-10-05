#lang typed/racket

(provide input-move-deltas)

(require "../../console.rkt")

(: input-move-deltas (-> Symbol (Values Integer Integer)))
(define (input-move-deltas a-key)
  (cond [(eq? a-key 'UP) (values 0 -1)]
        [(eq? a-key 'DOWN) (values 0 1)]
        [(eq? a-key 'LEFT) (values -1 0)]
        [(eq? a-key 'RIGHT) (values 1 0)]
        [else (values 0 0)]))
