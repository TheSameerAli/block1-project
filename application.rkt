#lang racket/gui

(require "traffic-light.rkt" "ped-light.rkt" "wait-light.rkt")

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

(define light-1 (new traffic-light% [dc dc] [x 100] [y 200] [state 0]))
(define light-2 (new traffic-light% [dc dc] [x 300] [y 300] [state 0]))
(define light-3 (new traffic-light% [dc dc] [x 500] [y 200] [state 0]))
(define ped-light (new ped-light% [dc dc] [x 900] [y 200] [state 0]))
(define wait-light (new wait-light% [dc dc] [x 900] [y 390] [state 0]))


;; Main application functionality
(define is-running #t)

(define (start-simulation . ignored-args)
    (send wait-light set-state 0)
    (send wait-light set-state 1)
    (sleep/yield 2)
    (send wait-light set-state 0)
    (cond 
      [(and is-running #t) 
        (start-simulation)   
      ]
    )
)

(define (initiate-traffic-lights dc) 
  (send light-1 render)
  (send light-2 render)
  (send light-3 render)
  (send ped-light render)
  (send wait-light render)
)

(define my-box
  ( new choice% [ parent frame ]
       [label " SELECT "]
       [choices (list "DAY" "NIGHT")]))


(define start-button (new button% 
  [parent frame] 
  [label "Start Simulation"] 
  [callback start-simulation]
))
(define stop-button (new button% [parent frame] [label "Stop and Reset"]))




;; Runs the main application
(define (start-app) 
    (send frame show #t)
    (initiate-traffic-lights dc)
    
)