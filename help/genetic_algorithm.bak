#lang racket
(provide genetic
         genetic-env
         fit-proportional-selection)

(struct genetic-env (
                     ff ; (chro) -> real
                     stop ; (best-prev-fitness, best-cur-fitness, iter) -> bool
                     better ; it must be < or >
                     init-population ; () -> list of chromosomes of length po-size
                     crossover ; (chro, chro) -> (chro, chro)
                     mutate ; (chro) -> chro
                     select ; (list of chro, fitnesses) -> (chro, chro)
                     po-size ; a natural defining the size of the populaiton
                    ))

(define (fitnesses e population)
  (map (genetic-env-ff e) population))

(define (stop e prev-opt-fs cur-opt-fs steps)
  ((genetic-env-stop e) prev-opt-fs cur-opt-fs steps))

(define (better e x y)
  ((genetic-env-better e) x y))

(define (init-po e)
  ((genetic-env-init-population e)))

; genetic algorithm core, takes in input a population and a genetic-env and iterates stop-condition
; and generation of a new population
(define (genetic e)
  (define (gen e po prev-fs steps)  
    (let ((fs (fitnesses e po)))
      (if (stop e (apply max prev-fs) (apply max fs) steps)
          (optimal-arg po fs (genetic-env-better e)) ; stop condition is true, return best individual
          (gen e (new-population e po fs) fs (add1 steps))))); go on, generate a new population
  ; the first fitness is all 0
  (gen e (init-po e) (build-list (genetic-env-po-size e) (lambda (x) (* 0 x))) 0)) 

; takes in input the elements list and the values list,
; and a comparison function between values
; (better x y) => True if x is better than y
(define (optimal-arg elements values better)
  (let ((e (car elements))
        (es (cdr elements))
        (v (car values))
        (vs (cdr values)))
    (if (null? es) ; end of the element list, return
        e
        (if (better v (car vs))
            (optimal-arg (cons e (cdr es)) (cons v (cdr vs)) better)
            (optimal-arg es vs better)))))

; generate a new population using the functions in a
; genetic environment
(define (new-population env population fitnesses)
  (let ((new (reproduction env population fitnesses)))
    (mutations env new)))

; take a list of chromosomes and return a list of chromosome with some reproduction
(define (reproduction env old-population fitnesses)
  (define (rep env old new fit step)
    (if (= 0 step)
        new
        (rep env old
             (append new ((genetic-env-crossover env) ((genetic-env-select env) old fitnesses)))
             fit (sub1 step))))
  (rep env old-population '() fitnesses (/ (genetic-env-po-size env) 2)))

; take a list of chromosomes and return the same list with some mutated
(define (mutations env population)
  (define (mut env old new)
    (if (null? old)
        new
        (cons ((genetic-env-mutate env) (car old)) (cdr old))))
  (mut env population '()))

; SELECTION METHODS

; select the parents proportionally to their fitness. Fitness function is assumed to be positive
(define (fit-proportional-selection po fs)
  (let* ((sum (foldl + 0 fs)) ; note that this assume the ff to be always positive
         (ps (map (lambda (x) (/ x sum)) fs)))
    ; given a random n in [0, 1] iterate over the elements of the list
    ; until the cumulated probability is greater or equal to n
    (define (get-rnd po ps acc n)
      (if (> acc n) ; acc need to start from the probability value of the first element, not 0
          (car po)
          (get-rnd (cdr po) (cdr ps) (+ acc (car ps)) n)))
    ; return a pair
    (cons (get-rnd po (cdr ps) (car ps) (random))
          (cons (get-rnd po (cdr ps) (car ps) (random)) '()))))