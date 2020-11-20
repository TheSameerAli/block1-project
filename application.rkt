#lang racket/gui

;; #### Main application #####
;; Authors: Sameer Ali
;; #############################################
;;                DESCRIPTION
;; #############################################
;; This file contains all the main logic to create windows,
;; perform GUI manipulations and control the traffic light 
;; logic.
;; #############################################

;; Import all the components/classes
(require "traffic-light.rkt" "ped-light.rkt" "wait-light.rkt" "scene.rkt")


;; Export all the variables off the scipt
;; so it can be accessed by other scripts
(provide start-app)


;; Setup the window for our application
;; Width = 1100px ; Height 600px
(define frame (new frame% [label "Traffic Light Simulation"] 
                   [width 1100]
                   [height 600]
                   [min-width 1100]
                   [min-height 600]
                   [stretchable-width #f]
                   [stretchable-height #f]
                   ))
                   
;; Draws a canvas on the window which lets us define 
;; a device context (dc) which allows us to draw on the screen
(define canvas (new canvas% [parent frame] 
                            [paint-callback (λ (can dc) 
                            (initiate-traffic-lights dc))]))
;; Creating a device context. This is used by Racket GUI library
;; to draw on the canvas
(define dc (send canvas get-dc))

;; Creates a pane for user interaction buttons to be placed in
;; This panel will contain all the buttons user can use to control
;; the simuation
(define buttons-pane (new horizontal-pane% 
    [parent frame]
    [stretchable-height #f]
    [min-width   300]
    [min-height   10]
    [alignment '(left bottom)]
))

;; Defining all the drawable objects in the program.
;; This include anything that is displayed on the screen (in white background)
;; Includes: Traffic Lights, Pedestrain Lights, Pedestrain Wait Light and scene info (road name)
(define light-1 (new traffic-light% [dc dc] [x 100] [y 120] [state 0] [label "(X)"]))
(define light-2 (new traffic-light% [dc dc] [x 300] [y 220] [state 0] [label "(Y)"]))
(define light-3 (new traffic-light% [dc dc] [x 500] [y 120] [state 0] [label "(Z)"]))
(define ped-light (new ped-light% [dc dc] [x 900] [y 120] [state 0]))
(define wait-light (new wait-light% [dc dc] [x 900] [y 310] [state 0]))
(define scene (new scene% [dc dc]))

;; Create a list containing all main traffic light to have better
;; algorithm to control the traffic light sequence
(define traffic-light-list (list 'light1 'light2 'light3))


;; ######### Variable Configuration Start ###############

;; Time it takes from get from one state to another if the state is non-green
;; For example, the time taken for the traffic light to go from green to amber or
;; amber to red or red to amber
(define transition-time 2) 

;; ######### Variable Configuration End #################


;; ######### Application States ####################
;; Description:
;; Not to be confused with Traffic Light States. This variables
;; control what is happening during the application is running.
;; These are then used to determine how to run the application
;; if the state was in a certain position.

; This variable stores which the last traffic light that showed green
; before the pedestrain stop light kicked in. This will help continue 
; the sequence it left from.
; Initial state is 'light1 as that is where it start from
(define last-traffic-light 0) 

;; This determines if the simulation is running or not.
;; When start simulation button is pressed, this is turned to true.
;; When stop simulation button is pressed, this is turned to false.
(define is-running #f)

;; Saves the current time of the day
;; This determins how many seconds light stays green for
;; depending on the time of the day
;; Day = 5 seconds, Night = 10 seconds
;; 0 = Day , 1 = Night
(define current-time 0)

;; ######### End Application State ##################



;; Below is the dictionary which keeps track of the states
;; of all the lights displayed on the screen. These are then
;; used to update display and control the sequence
(define states '(
  (light1 . 0) ;; State of traffic light 1
  (light2 . 0) ;; State of traffic light 2
  (light3 . 0) ;; State of traffic light 3
  (pedlight . 0) ;; State of Pedestrain traffic light
  (pedwaitlight . 0) ;; State of Pedestrain wait light
))


;; ############# Start Helper Functions ###############

;; Defining while syntax rule to use while loops in this program.
;; This is used as a syntatical sugar for recursion.
(define-syntax-rule (while condition body)   
  (let loop ()
    (when condition
      (body)
      (loop))))

;; The function below takes the last green light
;; and reorders the traffic light list to put
;; the light that is meant to run after the last light
;; in the first order to run next. This fixes the issue
;; which can let pedestrain wait light restart the whole sequence
;; of traffic light
(define (put-last-light-first) 
  (while (not (equal? last-traffic-light (first traffic-light-list))) (λ () 
      (set! traffic-light-list (append (rest traffic-light-list) (list (first (reverse traffic-light-list)))))
    )
  )
  (set! traffic-light-list (append (rest traffic-light-list) (list (first traffic-light-list))))
)
;; ############## End Helper Functions ################



;; ############## Threads Start #######################
;; Initialise threads where all the light loops will run.
;; Threads are being used to keep the UI from being frozen while
;; loops run in the background to change states.
(define traffic-light-thread (thread (λ () (println "traffic-light-thread-initialised"))))
(define ped-crossing-thread (thread (λ () (println "ped-crossing-thread-initialised"))))
(define ped-wait-button-thread (thread (λ () (println "ped-wait-button-thread-initialised"))))
(define switch-all-to-red-thread (thread (λ () (println "switch-all-to-red-thread-initialised"))))
;; ############## Thread Ends ##########################


;; ############### Main Traffic Light Simulation Logic ######################
(define (start-traffic-light-loop)
  (set! traffic-light-thread (thread (λ () 

      (while (and is-running #t) (λ () 
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
  (println (string-append (~v light) " >> " (~v state)))
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

(define stop-and-reset-simulation (λ (button event) 
    (set! is-running #f)
    (thread-suspend traffic-light-thread)
    (thread-suspend ped-crossing-thread)
    (thread-suspend switch-all-to-red-thread)
    (thread-suspend ped-wait-button-thread)

    (last-traffic-light 0)
  
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
  (send scene render)
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





(define (are-all-red)
  (and 
  (= (dict-ref states 'light1) 0) 
  (= (dict-ref states 'light2) 0) 
  (= (dict-ref states 'light3) 0))
)

(define (switch-all-to-red callback) 
  (set! switch-all-to-red-thread (thread (λ ()

      (while (and (not (are-all-red)) #t) (λ ()
        
        ;; Checks for light 1 state and switches it off
        (while (and (not (= (dict-ref states 'light1) 0)) #t) (λ () 
          (change-state 'light1 (send light-1 next-state (dict-ref states 'light1)))
          (sleep transition-time)
        ))

        ;; Checks for light 2 state and switches it off
        (while (not (= (dict-ref states 'light2) 0)) (λ () 
          (change-state 'light2 (send light-2 next-state (dict-ref states 'light2)))
          (sleep transition-time)
        ))

        ;; Checks for light 3 state and switches it off
        (while (not (= (dict-ref states 'light3) 0)) (λ () 
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



;; Drop-down menu which provides user to choose
;; the time of the day. Day and Night options are
;; available to choose which alters the time when
;; the traffic light is green
(define time-choice
  ( new choice% [ parent buttons-pane ]
       [callback change-time-choice]
       [label " SELECT "]
       [choices (list "DAY" "NIGHT")]))




;; ####### Start User Controls Here #####

;; Start simulation button
;; This will start the simulation from
;; it's initial state
(define start-button (new button% 
  [parent buttons-pane] 
  [label "Start Simulation"] 
  [callback start-simulation]
))


;; Stop and Reset button
;; This will stop and reset the current state
;; of the simulation
(define stop-button (new button% 
    [parent buttons-pane] 
    [label "Stop and Reset"]
    [callback stop-and-reset-simulation]
))

;; Pedestrain wait button
;; Pressing this button will turn on the pedestrain
;; wait light and wait for 5 seconds for all traffic
;; lights to go red and turn the pedestrain traffi
;; light to green
(define ped-wait-button (new button% 
  [parent buttons-pane]
  [label "Pedestrain Wait"]
  [callback (λ (button event) 
      (set! ped-wait-button-thread (thread (λ () 
          (change-state 'pedwaitlight 1)
          (sleep/yield 5)
          (thread-suspend traffic-light-thread)
          (switch-all-to-red (λ () 
            ;; Once all the traffic lights are switched to red
            (change-state 'pedwaitlight 0)
            (start-ped-crossing)
          ))
      )))
      
      
  )]
))
;; ######### User controls ends here #########



;; Runs the main application
(define (start-app) 
    (send frame show #t)
    (initiate-traffic-lights dc)
    
)