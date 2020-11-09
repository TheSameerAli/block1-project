#lang racket/gui

(require "traffic-light.rkt")

;; Export all the variables off the scipt
;; so it can be accessed by other scripts
(provide start-app)


;; Setup the window for our application
;; Width = 1200px ; Height 800px
(define frame (new frame% [label "Traffic Light Simulation"] 
                   [width 1200]
                   [height 800]))

;; Draws a canvas on the window which lets us define 
;; a device context (dc) which allows us to draw on the screen
(define canvas (new canvas% [parent frame] 
                            [paint-callback (λ (can dc) 
                            (initiate-traffic-lights dc))]))
(define dc (send canvas get-dc))

(define light-1 (new traffic-light% [dc dc] [x 100] [y 100] [state 0]))

(define (initiate-traffic-lights dc) 
  (send light-1 render)
)

;; Runs the main application
(define start-app (
    (send frame show #t)
    (initiate-traffic-lights dc)
    (sleep/yield 2)
    (send light-1 set-state 1)
    (sleep/yield 2)
    (send light-1 set-state 2)
    (sleep/yield 2)
    (send light-1 set-state 3)
    (sleep/yield 2)
    (send light-1 set-state 0)
  )
)