#lang racket/gui

(provide wait-light%)

;; 0 = Off
;; 1 = On


(define wait-light%
    (class object%
        (super-new)
        (init-field dc x y state)

        (define no-pen (make-object pen% "BLACK" 1 'transparent))
        (define no-brush (make-object brush% "BLACK" 'transparent))
        (define black-brush (make-object brush% "BLACK" 'solid))
        (define black-pen (make-object pen% "BLACK" 2 'solid))

        (define/public (set-state s) 
            (set! state s)
            (refresh-lights)
        )

        (define (refresh-lights)
            (draw-lights)
        )

        (define (draw-lights)
            (define light-x (+ x 15))
            (define light-y (+ y 10))
            (send dc set-font (make-font #:size 24 #:family 'default
                             #:weight 'bold))
        
            ;; Draws the red light (only if state is 0)
            (cond 
                [(= state 0) 
                    (send dc set-text-foreground (make-object color% 184 184 184))
                    (send dc draw-text "W A I T" light-x light-y)
                ]
                [else 
                    (send dc set-text-foreground (make-object color% 230 222 0))
                    (send dc draw-text "W A I T" light-x light-y)
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
            (send dc draw-rectangle x y 150 60)
        )
    )
)