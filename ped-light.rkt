#lang racket/gui

;; #### PEDESTRIAN Traffic Light Component #####
;; Authors: Sameer Ali, Jeremy Arland
;; #############################################
;;                DESCRIPTION
;; #############################################
;; This class allows us to create instances of pedestrain
;; traffic light light and render them on the screen. This class also
;; provide methods to change the light states which update the
;; graphics in real time
;; ##############################################
;;                 STATES
;;                0 = RED
;;                1 = GREEN
;; ###############################################

;; Export this class so it can used by other scripts

(provide ped-light%)


(define ped-light%
    (class object%
        (super-new)
        (init-field dc x y state) ;; Constructor method accepting variables

        ;; Initialising brushes and pens to be used for drawing
        (define no-pen (make-object pen% "BLACK" 1 'transparent))
        (define no-brush (make-object brush% "BLACK" 'transparent))
        (define black-brush (make-object brush% "BLACK" 'solid))
        (define grey-brush (make-object brush% (make-object color% 184 184 184) 'solid))
        (define red-brush (make-object brush% "RED" 'solid))
        (define green-brush (make-object brush% "GREEN" 'solid))
        (define black-pen (make-object pen% "BLACK" 2 'solid))

        ;; Publicly exposed method to allow states to be changed from other scripts
        (define/public (set-state s) 
            (set! state s)
            (refresh-lights)
        )

        ;; Method used to refresh the lights (re-draw) them.
        ;; Used when states are changed
        (define (refresh-lights)
            (draw-lights)
        )

        ;; Main method which draws the lights
        (define (draw-lights)
            (define light-x (+ x 30)) ; x-axis position of lights (circles)
            (define red-y (+ y 15)) ; y-axis for red light position
            (define green-y (+ red-y 80)) ; y-axis for green light position
            (define light-size 60) ; size of the light (circle)
        
            ;; Draws the red light (only if state is 0)
            (cond 
                [(= state 0) 
                    (send dc set-brush red-brush)
                    (send dc draw-ellipse light-x red-y light-size light-size)
                ]
                [else 
                    (send dc set-brush grey-brush)
                    (send dc draw-ellipse light-x red-y light-size light-size)
                ]
            )

            ;; Draws the green light (only if state is 1)
            (cond 
                [(= state 1) 
                    (send dc set-brush green-brush)
                    (send dc draw-ellipse light-x green-y light-size light-size)
                ]
                [else 
                    (send dc set-brush grey-brush)
                    (send dc draw-ellipse light-x green-y light-size light-size)
                ]
            )
        )
        

        (define/public (render) 
            ;; Reset the brushes
            (send dc set-pen no-pen)
            (send dc set-brush no-brush)

            ;; Calls the procedure to draw the traffic light box
            (draw-box)

            ;; Calls the procedure to draw the lights
            (draw-lights)
        )

        (define (draw-box) 
            ;; Sets a black pen and draws a container
            ;; for traffic lights (a box)
            (send dc set-pen black-pen)
            (send dc set-brush black-brush)
            (send dc draw-rectangle x y 120 180)
        )
    )
)