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
(define canvas (new canvas% [parent frame] 
                            [paint-callback (λ (can dc) 
                            (initiate-traffic-lights dc))]))
(define dc (send canvas get-dc))


(define (initiate-traffic-lights dc) 
  (define light-1 (new traffic-light% [dc dc] [x 100] [y 100] [state 2]))
  (send light-1 render)
)

;; Runs the main application
(define start-app (
    (send frame show #t)
    (initiate-traffic-lights dc)
  )
)