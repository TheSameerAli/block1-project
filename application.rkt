#lang racket/gui

(require "traffic-light.rkt")

;; Export all the variables off the scipt
;; so it can be accessed by other scripts
(provide start-app)


;; Setup the window for our application
;; Width = 900px ; Height 500px
(define frame (new frame% [label "Traffic Light Simulation"] 
                   [width 1200]
                   [height 800]))

;; Draws a canvas on the window which lets us define 
;; a device context (dc) which allows us to draw on the screen
(define canvas (new canvas% [parent frame]))
(define dc (send canvas get-dc))


;; Runs the main application
(define start-app (
    (send frame show #t)
    (sleep/yield 1)
    (render-traffic-light dc 100 100 0)
    (render-traffic-light dc 300 200 3)
    (render-traffic-light dc 500 100 1)
    (render-traffic-light dc 700 100 2)
  )
)
