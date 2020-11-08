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
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

(define (render-traffic-light dc) 
      (send dc set-pen no-pen)
      (send dc set-brush blue-brush)
      (send dc draw-ellipse 50 50 200 200)
    
      (send dc set-brush yellow-brush)
      (send dc draw-rectangle 100 100 10 10)
      (send dc draw-rectangle 200 100 10 10)
    
      (send dc set-brush no-brush)
      (send dc set-pen red-pen)
      (let ([-pi (atan 0 -1)])
        (send dc draw-arc 75 75 150 150 (* 5/4 -pi) (* 7/4 -pi)))
)