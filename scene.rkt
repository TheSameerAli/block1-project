#lang racket/gui

;; #### Scene Component #####
;; Authors: Sameer Ali
;; #############################################
;;                DESCRIPTION
;; #############################################
;; This class provides a overall scene class.
;; This class is responsible for drawing extra information
;; to give this simulation give more context
;; ##############################################

(provide scene%)


(define scene%
    (class object%
        (super-new)
        (init-field dc)

        (define no-pen (make-object pen% "BLACK" 1 'transparent))
        (define no-brush (make-object brush% "BLACK" 'transparent))
        (define black-brush (make-object brush% "BLACK" 'solid))
        (define black-pen (make-object pen% "BLACK" 2 'solid))


        (define/public (render) 
            ;; Reset the brushes
            (send dc set-pen no-pen)
            (send dc set-brush no-brush)

            ;; Set a black pen
            (send dc set-pen black-pen)
            (draw-road-name)
        )

        (define (draw-road-name)
            (send dc set-font (make-font #:size 18 #:family 'default
                             #:weight 500))
            (define road-name "Burroughs Gardens, NW4 4AU")
            (send dc set-text-foreground (make-object color% 0 0 0))
            (send dc draw-text (string-append "Simulation for: " road-name) 10 10)
        )
    )
)