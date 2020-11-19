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

(define traffic-light-list (list 'light1 'light2 'light3))

(define transition-time 2) ;; Time it takes from get from one state to another if the state is non-green

; This variable stores which the last traffic light that showed green
; before the pedestrain stop light kicked in. This will help continue 
; the sequence it left from.
; Initial state is 'light1 as that is where it start from
(define last-traffic-light 0) 

;; Main application functionality
(define is-running #f)

;; Saves the current time of the day
;; This determins how many seconds light stays green for
;; depending on the time of the day
;; Day = 5 seconds, Night = 10 seconds
;; 0 = Day , 1 = Night
(define current-time 0)

;; Initial states
(define states '(
  (light1 . 0) ;; State of traffic light 1
  (light2 . 0) ;; State of traffic light 2
  (light3 . 0) ;; State of traffic light 3
  (pedlight . 0) ;; State of Pedestrain traffic light
  (pedwaitlight . 0) ;; State of Pedestrain wait light
))

;; Comment the below
(define-syntax-rule (while-loop condition body)   
  (let loop ()
    (when condition
      (body)
      (loop))))

(define traffic-light-thread (thread (λ () (println "traffic-light-thread-initialised"))))
(define ped-crossing-thread (thread (λ () (println "ped-crossing-thread-initialised"))))
(define switch-all-to-red-thread (thread (λ () (println "switch-all-to-red-thread-initialised"))))

;; The function below takes the last green light
;; and reorders the traffic light list to put
;; the light that is meant to run after the last light
;; in the first order to run next. This fixes the issue
;; which can let pedestrain wait light restart the whole sequence
;; of traffic light
(define (put-last-light-first) 
  (while-loop (not (equal? last-traffic-light (first traffic-light-list))) (λ () 
      (set! traffic-light-list (append (rest traffic-light-list) (list (first (reverse traffic-light-list)))))
    )
  )
  (set! traffic-light-list (append (rest traffic-light-list) (list (first traffic-light-list))))
)

(define (start-traffic-light-loop)
  (set! traffic-light-thread (thread (λ () 

      (while-loop (and is-running #t) (λ () 
        (cond 
          [(not (and (number? last-traffic-light) (= last-traffic-light 0)))
            (put-last-light-first)
          ]
        )
        (for ([traffic-light traffic-light-list])
          ;; Runs the traffic light loop
          ;; going from state 1->2->3->0
          (change-state traffic-light 1)
          (sleep transition-time)
          (change-state traffic-light 2)
          (set! last-traffic-light traffic-light) ;; Sets the current traffic light to last traffic light state
          (cond 
            [(= current-time 0) 
              (sleep 5)
            ]
            [(= current-time 1) 
              (sleep 10)
            ]
          )
          (change-state traffic-light 3)
          (sleep transition-time)
          (change-state traffic-light 0)
          (sleep transition-time)
        )
      ))
    ))
))
  


(define (change-state light state)
  ;(println (string-append (~v light) " >> " (~v state)))
  (cond 
    [(number? state) 
        (set! states (dict-set states light state))
        (refresh-lights)
    ]
    [else 
      (println "State can only be a number")
    ]
  )
  
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
    (start-traffic-light-loop)
  )  
)

(define stop-simulation (λ (button event) 
    (set! is-running #f)
    (thread-suspend traffic-light-thread)
    (thread-suspend ped-crossing-thread)
    (thread-suspend switch-all-to-red-thread)
  
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

(define change-time-choice (λ (choice event)
  (cond 
    [(or (= (send choice get-selection) 0) (= (send choice get-selection) 1))
      (set! current-time (send choice get-selection))
    ]
    [else 
      (println "Invalid time selection")
    ]
  )
))


(define time-choice
  ( new choice% [ parent frame ]
       [callback change-time-choice]
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

(define (are-all-red)
  (and 
  (= (dict-ref states 'light1) 0) 
  (= (dict-ref states 'light2) 0) 
  (= (dict-ref states 'light3) 0))
)

(define (switch-all-to-red callback) 
  (set! switch-all-to-red-thread (thread (λ ()

      (while-loop (and (not (are-all-red)) #t) (λ ()
        
        ;; Checks for light 1 state and switches it off
        (while-loop (and (not (= (dict-ref states 'light1) 0)) #t) (λ () 
          (change-state 'light1 (send light-1 next-state (dict-ref states 'light1)))
          (sleep transition-time)
        ))

        ;; Checks for light 2 state and switches it off
        (while-loop (not (= (dict-ref states 'light2) 0)) (λ () 
          (change-state 'light2 (send light-2 next-state (dict-ref states 'light2)))
          (sleep transition-time)
        ))

        ;; Checks for light 3 state and switches it off
        (while-loop (not (= (dict-ref states 'light3) 0)) (λ () 
          (change-state 'light3 (send light-3 next-state (dict-ref states 'light3)))
          (sleep transition-time)
        ))

      ))
      (callback)
  
    )
  ))
)

(define (start-ped-crossing) 
    (thread-suspend traffic-light-thread)

    (set! ped-crossing-thread (thread (λ ()
        (change-state 'pedlight 1)
        (sleep 5)
        (change-state `pedlight 0)
        (sleep transition-time)
        (start-traffic-light-loop)
        (thread-suspend ped-crossing-thread)
    )))
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
      (thread-suspend traffic-light-thread)
      (switch-all-to-red (λ () 
        ;; Once all the traffic lights are switched to red
        (change-state 'pedwaitlight 0)
        (start-ped-crossing)
      ))
      
  )]
))




;; Runs the main application
(define (start-app) 
    (send frame show #t)
    (initiate-traffic-lights dc)
    
)