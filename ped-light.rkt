#lang racket/gui

(provide ped-light%)

;; 0 = Red
;; 1 = Green


(define ped-light%
    (class object%
        (super-new)
        (init-field dc x y state)

        (define no-pen (make-object pen% "BLACK" 1 'transparent))
        (define no-brush (make-object brush% "BLACK" 'transparent))
        (define black-brush (make-object brush% "BLACK" 'solid))
        (define grey-brush (make-object brush% (make-object color% 184 184 184) 'solid))
        (define red-brush (make-object brush% "RED" 'solid))
        (define green-brush (make-object brush% "GREEN" 'solid))
        (define black-pen (make-object pen% "BLACK" 2 'solid))

        (define/public (set-state s) 
            (set! state s)
            (refresh-lights)
        )

        (define (refresh-lights)
            (draw-lights)
        )

        (define (draw-lights)
            (define light-x (+ x 30))
            (define red-y (+ y 15))
            (define green-y (+ red-y 80))
            (define light-size 60)
        
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