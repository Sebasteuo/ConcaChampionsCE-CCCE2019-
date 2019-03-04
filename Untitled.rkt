#lang racket



(require (lib "graphics.ss" "graphics"))
(open-graphics)
 
(define ventana1 (open-viewport "ConcaChampionsCE (CCCE2019)" 900 300))



(((draw-pixmap-posn "images/cancha3.png" 'png/mask) ventana1)(make-posn 0 0 ))
