;Christian Moreano
;CS 152
;Quadratic Solver:given three parameters
; a b c , solve for x -> ax2 + bx + c = 0 

#lang scheme

;disc function calculates b^2-4ac
(define (disc a b c) ( - (* b b) (* 4 a c)))

;quadratic_solution returns a list composed of solutions x1 , x2
(define ( quadratic_solution A B C)
; if disc < 0 , then print no real solution
        ( cond [( < (disc A B C) 0) "No real solution."]
               
              ; if the above condition is not met
              ; place and return x1,x2 from a list
              [
               else 
                     (
                       list(/ (+(- B) (sqrt(disc A B C))) (* 2 A))
                           (/ (-(- B) (sqrt(disc A B C))) (* 2 A))
                     )
              ]
        );end of if else statement
 );end of quadratic_solution