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
(define light-1 (new traffic-light% [dc dc] [x 100] [y 120] [state 0] [label "(Y)"]))
(define light-2 (new traffic-light% [dc dc] [x 300] [y 220] [state 0] [label "(Z)"]))
(define light-3 (new traffic-light% [dc dc] [x 500] [y 120] [state 0] [label "(X)"]))
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
;; #################################################

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
      ;; [light1, light2, light3] -> [light2, light3, light1] -> [light3, light1, light2]
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

;; The procedure below kicks in the traffic light simulation loop.
;; This procedure starts the traffic light loops and keeps it running
;; forever until interrupted by the user in any form.
(define (start-traffic-light-loop)
  (set! traffic-light-thread (thread (λ ()  ;; Sets this loop to a thread to prevent GUI freezing

      (while (and is-running #t) (λ () ;; While "is-running" variable is true
        (cond 

          ;; The condition below checks if the last-traffic-light was not 0
          ;; If the traffic light is 0, that means it's either the first time 
          ;; running the simulation or running it from reset state.
          ;; If it's not 0, it takes the last traffic light green and continue
          ;; with the next one.
          [(not (and (number? last-traffic-light) (= last-traffic-light 0)))
            (put-last-light-first)
          ]
        )
        (for ([traffic-light traffic-light-list]) ;; For loop to go through all traffic lights
          ;; Runs the traffic light loop
          ;; going from state 1->2->3->0
          (change-state traffic-light 1) ;; Changes the state to Red+Amber
          (sleep transition-time)
          (change-state traffic-light 2) ;; Changes the state to Green
          (set! last-traffic-light traffic-light) ;; Sets the current traffic light to last traffic light state
          (cond 
            [(= current-time 0) ;; Checks the current-time selected by user. If Day: 
              (sleep 5) ;; Then wait at green for 5 seconds
            ]
            [(= current-time 1) ;; If Night:
              (sleep 10) ;; Then wait at green for 10 seconds
            ]
          )
          (change-state traffic-light 3) ;; Change the state to Amber
          (sleep transition-time)
          (change-state traffic-light 0) ;; Change the state to Red
          (sleep transition-time)
        )
      ))
    ))
))
  

;; The procedure below is responsible
;; for taking in light and state as parameters
;; and changing them
(define (change-state light state)
  (println (string-append (~v light) " >> " (~v state))) ;; Prints the state of light
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

;; Refresh lights procedure is responsible
;; for re-drawing the lights on the screen
;; once their state has been changed
;; This works by calling a method in each of the light
;; class to change their state which automatically
;; redraws the light on the screen
(define (refresh-lights) 
  (send light-1 set-state (dict-ref states 'light1))
  (send light-2 set-state (dict-ref states 'light2))
  (send light-3 set-state (dict-ref states 'light3))
  (send ped-light set-state (dict-ref states 'pedlight))
  (send wait-light set-state (dict-ref states 'pedwaitlight))
)

;; ########## Call back procedures ###############
;; Description:
;; The procedures below are the callback procedures
;; to run when triggered by user interaction
;; ############################################

;; The procedure below runs when the "start simulation"
;; button is clicked. It sets "is-running" variable to true
;; and starts the traffic light loop.
(define start-simulation (λ (button event) 
    (set! is-running #t)
    (start-traffic-light-loop)
  )  
)

;; The procedure belows run when the "stop and reset simulation"
;; button is clocked. This procedure stop all the running threads
;; and sets the "is-running" variable to false. It also resets the 
;; program states such as last-traffic-light and light states.
(define stop-and-reset-simulation (λ (button event) 
    (set! is-running #f)
    (thread-suspend traffic-light-thread)
    (thread-suspend ped-crossing-thread)
    (thread-suspend switch-all-to-red-thread)
    (thread-suspend ped-wait-button-thread)

    (set! last-traffic-light 0)
  
    (reset-states)
  )
)

;; The procedure below is responisble for resetting
;; states of all the lights to 0. It does this by calling
;; a set state method in each of the light object.
(define (reset-states) 
  (send light-1 set-state 0)
  (send light-2 set-state 0)
  (send light-3 set-state 0)
  (send ped-light set-state 0)
  (send wait-light set-state 0)
)

;; The procedure below is responsible for rendering
;; the traffic lights on the screen by calling the
;; render method from each traffic light and drawable objects
(define (initiate-traffic-lights dc)
  (send scene render)
  (send light-1 render)
  (send light-2 render)
  (send light-3 render)
  (send ped-light render)
  (send wait-light render)
  (reset-states)
)

;; The method below runs when the user changes the option 
;; for the time of day dropdown menu.
;; This procedure sets the user choice to the global "current-time"
;; variable.
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

;; ############ End of callback procedures ##################


;; The function below checks if all the traffic
;; lights are red and return #t or #f based on the result.
(define (are-all-red)
  (and 
  (= (dict-ref states 'light1) 0) 
  (= (dict-ref states 'light2) 0) 
  (= (dict-ref states 'light3) 0))
)

(define (get-light-object-from-dict-ref dict-light)
  (cond 
    [(equal? dict-light 'light1)
      light-1
    ]

    [(equal? dict-light 'light2)
      light-2
    ]

    [(equal? dict-light 'light3)
      light-3
    ]
  )
)

;; The procedure below switches all the traffic lights
;; to red in order. The procedure checks for each traffic lights
;; and run a sequence to switch the light to red if it's not already.
;; This is run to get the traffic lights turned to red in order for the pedestrian
;; to cross the street.
(define (switch-all-to-red callback) 
  (set! switch-all-to-red-thread (thread (λ () ;; Starts the thread to prevent GUI from freezing

      (while (and (not (are-all-red)) #t) (λ () ;; While not all traffic light are red, the codeblock below runs
        (for ([traffic-light traffic-light-list]) ; Run through each traffic light and turn them red
            (while (and (not (= (dict-ref states traffic-light) 0)) #t) (λ () 
              (change-state traffic-light (send (get-light-object-from-dict-ref traffic-light) next-state (dict-ref states traffic-light))) ;; Switches to the next state in sequence
              (sleep transition-time) ;; Wait for a reasonable time before repeating the process
            ))
        )
      ))
      (callback)
    )
  ))
)

;; The procedure belows is run when all the traffic lights
;; are turned red and pedestrian are ready to cross.
(define (start-ped-crossing) 
    (thread-suspend traffic-light-thread) ;; Suspends the main traffic light loop

    (set! ped-crossing-thread (thread (λ ()
        (change-state 'pedlight 1) ;; Changes the pedestrian light to green
        (sleep 5) ;; Wait for 5 seconds for pedestrain to cross
        (change-state `pedlight 0) ;; Change the pedestrain light to red
        (sleep transition-time)
        (start-traffic-light-loop) ;; Starts the traffic light loop again
        (thread-suspend ped-crossing-thread) ;; Suspends this thread to stop pedestrain light from changing
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
;; by showing the window and calling 
;; the initiate traffic lights procedure
(define (start-app) 
    (send frame show #t)
    (initiate-traffic-lights dc)
    
)