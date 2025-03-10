#lang racket


(define (new.< x y )
    (< x y))

(define (new.> x y)
  (not (< x y))
  )

(define (new.= x y)
  (and (not (< x y)) (not (< y x)))
  )

(define (new.<= x y)
  (or (< x y) (new.= x y) )
)

(define (new.>= x y)
  (not (< x y) )
  )

(define (new.<> x y)
  (or (< x y) (< y x) )
)


(define (nwd a b)
  (if (= (remainder a b) 0)
    b
    (nwd b (remainder a b))
));


(define (nww a b)
  (/ (abs (* a b)) (nwd a b) )
  );


(define (odd? n)
  (if (zero? n) 
      #f
      (if (= n 1) #t (odd? (- n 2)))))

(define (even? n)
  (not (odd? n)))

(define (same-values? p1 p2 x y)
  (new.= (p1 x y) (p2 x y)))