#lang racket/gui

(provide render-traffic-light)

;; 0 = Red
;; 1 = Amber+Red
;; 2 = Green
;; 3 = Amber

(define lights-config (list
                       (list '())
                       (list '(7 "red") '(8 "orange") '(9 "green"))                    
                       )
  )

;; Define our brushes (for different colours)
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define red-brush (make-object brush% "RED" 'solid))
(define amber-brush (make-object brush% "YELLOW" 'solid))
(define green-brush (make-object brush% "GREEN" 'solid))
(define black-pen (make-object pen% "BLACK" 2 'solid))

(define (render-traffic-light dc x y)
        ;; Reset the brushes
        (send dc set-pen no-pen)
        (send dc set-brush no-brush)

        ;; Sets a black pen and draws a container
        ;; for traffic lights (a box)
        (send dc set-pen black-pen)
        (send dc draw-rectangle x y 150 270)

        ;; Co-ordinates of traffic lights
        ;; and sizes variables
        (define light-x (+ x 40))
        (define red-y (+ y 15))
        (define amber-y (+ red-y 80))
        (define green-y (+ amber-y 80))
        (define traffic-light-size 70)


        ;; Draws the red light
        (send dc set-brush red-brush)
        (send dc draw-ellipse light-x red-y traffic-light-size traffic-light-size)

        ;; Draws the amber light
        (send dc set-brush amber-brush)
        (send dc draw-ellipse light-x amber-y traffic-light-size traffic-light-size)

        ;; Drawas the green light
        (send dc set-brush green-brush)
        (send dc draw-ellipse light-x green-y traffic-light-size traffic-light-size)
)