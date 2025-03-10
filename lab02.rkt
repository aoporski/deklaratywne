#lang racket


(define (sil n)
(if (= n 0 ) 1
(* n (sil (- n 1 ))))
)

(define (sil-acc n)
  (define (helper n acc)
    (if (= n 0) acc
       (helper (- n 1) (* n acc))
        )
    )

(helper n 1 ))

;(sil 3) => (* 3 (sil 2) ) => ( * 3 (* 2 (sil 1 ))) =>  ( * 3 (* 2 (* 1 (sil 0))) => ( * 3 (* 2 (* 1 1))) => 6

;(sil-acc 3 ) => (helper 3 1) => (helper (2 3)) => (helper (1 6) ) => (helper (0 6 ) ) => 6


(define (fib n)
  (if (= n 0) 
      0
      (if (= n 1) 
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-acc n)
  (define (helper n acc b )
    (if (= n 0)
        acc
        (helper (- n 1) b (+ acc b) )
        )
        )
  
  (helper n 0 1)
)

; fib 3 => + fib 2 fib 1 => + fib 1 fib 0 1 => + 1 0 1 => 2

; fib-acc 3 => helper(3 0 1) => helper(2 1 1) => helper(1 1 2) => helper(0 2 3)

(define (exp b e)
  (if (= e 0) 
      1  
      (if (even? e)
          (* (exp b (/ e 2)) (exp b (/ e 2))) 
          (* b (exp b (- e 1))))))
  
(define (exp-acc b e)
  (define (exp-acc-helper b e acc)
    (if (= e 0)
        acc  
        (if (even? e)
            (exp-acc-helper (* b b) (/ e 2) acc)  
            (exp-acc-helper b (- e 1) (* b acc)))))  
  (exp-acc-helper b e 1))  

;(exp 2 6) 
;=> (* (exp 2 3) (exp 2 3)) 
;=> (* (* 2 (exp 2 2)) (* 2 (exp 2 2))) 
;=> (* (* 2 (* (exp 2 1) (exp 2 1))) (* 2 (* (exp 2 1) (exp 2 1)))) 
;=> (* (* 2 (* 2 (exp 2 0))) (* 2 (* 2 (exp 2 0)))) 
;=> (* (* 2 (* 2 1)) (* 2 (* 2 1))) 
;=> (* (* 2 2) (* 2 2)) 
;=> (* 4 4) 
;=> 64

;(exp-acc 2 6) 
;=> (exp-acc-helper 2 6 1) 
;=> (exp-acc-helper 4 3 1) 
;=> (exp-acc-helper 4 2 4) 
;=> (exp-acc-helper 16 1 4) 
;=> (exp-acc-helper 256 0 1024) 
;=> 1024 
;=> 64

(define (new-if warunek alternatywa1 alternatywa2)   (cond (warunek alternatywa1)         (else    alternatywa2)))
; if wymaga trzech argumentów, a cond składa się z warunku i wyników
