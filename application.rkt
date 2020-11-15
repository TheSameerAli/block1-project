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
(define is-running #f)

;; Initial states
(define states '(
  (light1 . 0) ;; State of traffic light 1
  (light2 . 0) ;; State of traffic light 2
  (light3 . 0) ;; State of traffic light 3
  (pedlight . 0) ;; State of Pedestrain traffic light
  (pedwaitlight . 0) ;; State of Pedestrain wait light
))

;; while loop recusive function
(define (while condition body) ;; defines the while procedure
  (when condition ;; checks if the condition is true
    (body)
    (while condition body))) ;; recalls the while function

(define main-thread (thread (λ () (println "main-thread-initialised"))))

(define (start-loop)
  (set! main-thread (thread (λ () 

      (while (and is-running #t) (λ () 
        (change-state 'light1 1)
        (sleep 2)
        (change-state 'light1 2)
        (sleep 5)
        (change-state 'light1 3)
        (sleep 2)
        (change-state 'light1 0)
        (sleep 2)

        (change-state 'light2 1)
        (sleep 2)
        (change-state 'light2 2)
        (sleep 5)
        (change-state 'light2 3)
        (sleep 2)
        (change-state 'light2 0)
        (sleep 2)

        (change-state 'light3 1)
        (sleep 2)
        (change-state 'light3 2)
        (sleep 5)
        (change-state 'light3 3)
        (sleep 2)
        (change-state 'light3 0)
        (sleep 2)
      ))
    ))
))
  


(define (change-state light state)
  (set! states (dict-set states light state))
  (refresh-lights)
)

(define (refresh-lights) 
  (send light-1 set-state (dict-ref states 'light1))
  (send light-2 set-state (dict-ref states 'light2))
  (send light-3 set-state (dict-ref states 'light3))
  (send ped-light set-state (dict-ref states 'pedlight))
  (send wait-light set-state (dict-ref states 'pedwaitlight))
)

(define start-simulation (λ (button event) 
    (set! is-running #t)
    (start-loop)
  )  
)

(define stop-simulation (λ (button event) 
    (set! is-running #f)
    (thread-suspend main-thread)
    (reset-states)
  )
)

(define (reset-states) 
  (send light-1 set-state 0)
  (send light-2 set-state 0)
  (send light-3 set-state 0)
  (send ped-light set-state 0)
  (send wait-light set-state 0)
)

(define (initiate-traffic-lights dc) 
  (send light-1 render)
  (send light-2 render)
  (send light-3 render)
  (send ped-light render)
  (send wait-light render)
  (reset-states)
)

(define my-box
  ( new choice% [ parent frame ]
       [label " SELECT "]
       [choices (list "DAY" "NIGHT")]))


;; Start simulation button
;; This will start the simulation from
;; it's initial state
(define start-button (new button% 
  [parent frame] 
  [label "Start Simulation"] 
  [callback start-simulation]
))

;; Stop and Reset button
;; This will stop and reset the current state
;; of the simulation
(define stop-button (new button% 
    [parent frame] 
    [label "Stop and Reset"]
    [callback stop-simulation]
))

(define (switch-all-to-red) 
  (thread (λ () 
    ;; Checks for light 1 state and switches it off
    (while (not (= (dict-ref states 'light1) 0)) (λ () 
      (change-state 'light1 (send light-1 next-state (dict-ref states 'light1)))
      (sleep 2)
    ))

    ;; Checks for light 2 state and switches it off
    (while (not (= (dict-ref states 'light2) 0)) (λ () 
      (change-state 'light2 (send light-2 next-state (dict-ref states 'light2)))
      (sleep 2)
    ))

    ;; Checks for light 2 state and switches it off
    (while (not (= (dict-ref states 'light3) 0)) (λ () 
      (change-state 'light3 (send light-1 next-state (dict-ref states 'light3)))
      (sleep 2)
    ))
  ))
  
)

;; Pedestrain wait button
;; Pressing this button will turn on the pedestrain
;; wait light and wait for 5 seconds for all traffic
;; lights to go red and turn the pedestrain traffi
;; light to green
(define ped-wait-button (new button% 
  [parent frame]
  [label "Pedestrain Wait"]
  [callback (λ (button event) 
      (change-state 'pedwaitlight 1)
      (sleep/yield 5)
      (thread-suspend main-thread)
      (switch-all-to-red)
      (change-state 'pedwaitlight 0)
      (change-state 'pedlight 1)
  )]
))




;; Runs the main application
(define (start-app) 
    (send frame show #t)
    (initiate-traffic-lights dc)
    
)