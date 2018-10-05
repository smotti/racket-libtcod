#lang typed/racket

(provide message-add
         message-log-entries
         message-wrap)

(require "../../color.rkt"

         "game-constants.rkt")


(define-type Entry (Pair String Color))

(: message-log (Boxof (Listof Entry)))
(define message-log (box '()))

(: message-log-entries (-> (Listof Entry)))
(define (message-log-entries)
  (unbox message-log))

(: message-wrap (-> String Integer (Listof String)))
(define (message-wrap a-message max-width)
  (define message-length (string-length a-message))
  (for/list ([w (range 0 message-length max-width)])
    (define end-index (+ w max-width))
    (substring a-message w (if (> end-index message-length)
                               message-length
                               end-index))))

(: message-add (-> String (#:color Color) Void))
(define (message-add a-message #:color [color color-white])
  (define message-lines (message-wrap a-message MSG-WIDTH))
  (define current-messages (unbox message-log))
  (define new-messages
    (for/fold : (Listof (Pair String Color))
        ([ms current-messages])
        ([m message-lines])
      (if (= (length ms) MSG-HEIGHT)
          (append (drop ms 1) (list (cons m color)))
          (append ms (list (cons m color))))))

  (set-box! message-log new-messages))
