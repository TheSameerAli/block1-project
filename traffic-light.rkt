#lang racket/gui

;; #### Main Traffic Light Component #####
;; Authors: Sameer Ali
;; #############################################
;;                DESCRIPTION
;; #############################################
;; This class allows us to create instances of main 3-light
;; traffic light and render them on the screen. This class also
;; provide methods to change the light states which update the
;; graphics in real time
;; ##############################################
;;                 STATES
;;                0 = RED
;;                1 = Amber+Red
;;                2 = Green
;;                3 = Amber
;; ###############################################


(provide traffic-light%)


(define traffic-light%
    (class object%
        (super-new)
        (init-field dc x y state)

        (define no-pen (make-object pen% "BLACK" 1 'transparent))
        (define no-brush (make-object brush% "BLACK" 'transparent))
        (define black-brush (make-object brush% "BLACK" 'solid))
        (define grey-brush (make-object brush% (make-object color% 184 184 184) 'solid))
        (define red-brush (make-object brush% "RED" 'solid))
        (define amber-brush (make-object brush% "YELLOW" 'solid))
        (define green-brush (make-object brush% "GREEN" 'solid))
        (define black-pen (make-object pen% "BLACK" 2 'solid))

        (define/public (set-state s) 
            (set! state s)
            (refresh-lights)
        )

        (define (refresh-lights)
            (draw-lights)
        )


        (define/public next-state (λ (cs) 
            (cond 
                [(= cs 0) 1]
                [(= cs 1) 2]
                [(= cs 2) 3]
                [(= cs 4) 0]
            )
        ))


        (define (draw-lights)
            (define light-x (+ x 40))
            (define red-y (+ y 15))
            (define amber-y (+ red-y 80))
            (define green-y (+ amber-y 80))
            (define light-size 70)
            ;; Draws the red light (only if state is 0 or 1)
            (cond 
                [(or (= state 0) (= state 1)) 
                    (send dc set-brush red-brush)
                    (send dc draw-ellipse light-x red-y light-size light-size)
                ]
                [else 
                    (send dc set-brush grey-brush)
                    (send dc draw-ellipse light-x red-y light-size light-size)
                ]
            )

            ;; Draws the amber light (only if state is 1 or 3)
            (cond 
                [(or (= state 1) (= state 3)) 
                    (send dc set-brush amber-brush)
                    (send dc draw-ellipse light-x amber-y light-size light-size)
                ]
                [else 
                    (send dc set-brush grey-brush)
                    (send dc draw-ellipse light-x amber-y light-size light-size)
                ]
            )

            ;; Draws the green light (only if state is 2)
            (cond 
                [(= state 2) 
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
            (send dc draw-rectangle x y 150 270)
        )
    )
)