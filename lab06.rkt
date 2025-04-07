#lang racket


(define (ewal expr values)
  (cond
    ((symbol? expr) (cdr (assoc expr values)))

    ((eq? (car expr) 'not)
     (not (ewal (cadr expr) values)))

    ((eq? (car expr) 'and)
     (and (ewal (cadr expr) values)
          (ewal (caddr expr) values)))

    ((eq? (car expr) 'or)
     (or (ewal (cadr expr) values)
         (ewal (caddr expr) values)))

    ((eq? (car expr) 'implies)
     (or (not (ewal (cadr expr) values))
         (ewal (caddr expr) values)))

    ((eq? (car expr) 'xor)
     (let ((a (ewal (cadr expr) values))
           (b (ewal (caddr expr) values)))
       (or (and a (not b)) (and (not a) b))))

    (else
     (error "Nieznany operator" (car expr)))))

(define w '((x . #f) (y . #t) (z . #f)))

(ewal '(and x (not y)) w) ; => #f
(ewal '(or x (not y)) w)  ; => #f
(ewal '(or y z) w)        ; => #t
(ewal '(implies x y) w)   ; => #t