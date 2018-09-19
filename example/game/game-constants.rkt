#lang typed/racket

(provide BAR-WIDTH
         MSG-HEIGHT
         MSG-WIDTH
         MSG-X
         PANEL-HEIGHT
         PANEL-Y
         SCREEN-HEIGHT
         SCREEN-WIDTH)

(define SCREEN-WIDTH 80)
(define SCREEN-HEIGHT 50)

(define BAR-WIDTH 20)
(define PANEL-HEIGHT 7)
(define PANEL-Y (- SCREEN-HEIGHT PANEL-HEIGHT))

(define MSG-X (+ BAR-WIDTH 2))
(define MSG-WIDTH (- SCREEN-WIDTH BAR-WIDTH 2))
(define MSG-HEIGHT (sub1 PANEL-HEIGHT))
