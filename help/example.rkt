;https://github.com/emilianogagliardi/genetic-algorithm-racket

#lang racket

(require "genetic_algorithm.rkt")

; a chromosome is a list of numbers in [0, 10], the fitness function is - sum
(define (create-env) 
(let ((po-size 30) ; crossover assume this to be even
      (chro-length 10)
      (crossover-probability 0.2)
      (crossover-mid 5)
      (mutation-probability 0.1)) 

  (define (ini-po)
    (define (ini size po)
      (if (= 0 size)
          po
          (ini (sub1 size) (cons (build-list chro-length (lambda (x) (random 10))) po))))
    (ini po-size '()))

  (define (crossover couple)
    (let ((x (car couple))
          (y (car (cdr couple))))
      (if (< (random) crossover-probability)
          (cons x (cons y '()))
          (cons
           (append (take x crossover-mid) (drop y crossover-mid))
           (cons (append (drop x crossover-mid) (take y crossover-mid)) '())))))

  (define (mutation chro)
    (define (mu chro pos index)
      (let ((x (car chro))
            (xs (cdr chro)))
        (if (null? xs)
            chro
            (if (= index pos)
                (cons (random 10) xs)
                (cons x (mu xs pos (add1 index)))))))
    (if (< (random) mutation-probability)
        (mu chro (random chro-length) 0)
        chro))

  (genetic-env
   (lambda (chro) (- 100 (foldl + 0 chro))) ; fitness function
   (lambda (pf cf s) (= 200000 s)) ; stop condition
   > ; bigger fitness is better
   ini-po
   crossover
   mutation
   fit-proportional-selection
   po-size)
  )
)

(let ((e (create-env)))
  (genetic e))