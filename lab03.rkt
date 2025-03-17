#lang racket

(define (sum term next a b) (
if(> a b) 0
(+ (term a) (sum term next (next a) b)))
)

(define (product term next a b)
  (if (> a b)
      1
      (* (term a) (product term next (next a) b))))

(define (licznik n)
  (product (lambda (x) (* x x)) 
          (lambda (x) (+ x 2)) 
          2    
          n)) 
(define (mianownik n)
  (product (lambda (x) (* x (+ x 2))) 
          (lambda (x) (+ x 2)) 
          1          
          (- n 1)))  

(define (pi-przybliz n)
  (* 2 (/ (licznik n) (mianownik n))))
  

