#lang racket

(provide distance-to
         input-move-deltas
         )

(require "../../console.rkt"

         "types.rkt")

(define (distance-to from to)
  (define dx (- (entity-x to) (entity-x from)))
  (define dy (- (entity-y to) (entity-y from)))
  (sqrt (+ (sqr dx) (sqr dy))))

(define (input-move-deltas a-key)
  (cond [(eq? a-key 'UP) (values 0 -1)]
        [(eq? a-key 'DOWN) (values 0 1)]
        [(eq? a-key 'LEFT) (values -1 0)]
        [(eq? a-key 'RIGHT) (values 1 0)]
        [else (values 0 0)]))
