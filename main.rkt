#lang racket

;
;
;===============================================
;
; (c) 2021
; version 1.0.0 2021-11-09
;
;
;-----------------------------------------------
; EIF400 Paradigmas de Programación
; 2do ciclo 2021, Grupo 50043 y 50042
; Proyecto 1
; 
;
; 207920434 Francisco Fernandez Rojas - Grupo 3pm
; 702670531 Yeikol Villalobos Herrera - Grupo 3pm 
; 117350395 William Arias Bermudez - Grupo 8pm
;
; ===============================================
;

;*************** Datos de prueba ***************

(define banco '(1 2 3 5 6 9 32 1 2 5 12 31 15)) 
(define characteristic '(30 60 90))
(define indv '(((2 6 2) 10) ((9 5 1 1 5) 21) ((3 31 15) 49) ((12 32) 44)))


(define (addList L)
   (if (empty? L) 0 
       (+ (car L) (addList (cdr L))) 
   )
)


(define getOptimum 
  (lambda (g L)
    (/ (addList L) g)
    )
  )


(define (breedCharacteristic L) 
  (if (empty? L) '()
      (cons L (list (addList L)))
   )
)



(define add 
  (lambda (x m A)
    (cond ((null? A) (list (list x)))
          ((= (remainder x m) (remainder (caar A) m))
           (cons (cons x (car A)) (cdr A)))
          (else
           (cons (car A) (add x m (cdr A)))))
    ))

(define listIndv-x
  (lambda (L m A)
    (if (null? L) A
        (listIndv-x (cdr L) m (add (car L) m A)))
    ))


(define listIndv
  (lambda ( m L)
    (listIndv-x (shuffle L) m '())
    ))


(define (breedIndv L)
  (if (empty? L) '()
      (cons (breedCharacteristic (car L)) (breedIndv (cdr L)))
      )
 )


(define (generateValues L)
  (if (empty? L)'()
      (cons (cadar L) (generateValues (cdr L)))
      )
 )


(define deductOptimum
  (lambda (L c)
    (map (lambda (x) (abs(- (getOptimum 4 L) x ))) (generateValues c)   )
    )
 )


(define (quicksort lst gen )
  (cond
    ((or (null? lst) 
         (null? (cdr lst))) 
     lst)
    (else
      (let ((pivot (addList (deductOptimum gen (car lst) ))) 
            (rest (cdr lst)))
        (append
          (quicksort 
            (filter (lambda (x) (< (addList (deductOptimum gen x )) pivot)) rest) gen) 
          (list pivot) 
          (quicksort 
            (filter (lambda (x) (>= (addList (deductOptimum gen  x)) pivot)) rest) gen)))))) 



(define firstGen 
  (lambda (a l g) 
    (if (zero? a) '()
        (cons
         (breedIndv (listIndv g l)) (firstGen (- a 1) l g)
        )
    )
  )
)


(print "INICIO")
(newline)
(print "================================================== ==================================================")
(newline)
(print "GENERACION INICIAL")
(newline)
(print "====================================================================================================")
(newline)
(firstGen 4 banco 4)
(print "====================================================================================================")
(newline)
(print "Individuo  optimo es: ")
(newline)
(quicksort (firstGen 4 banco 4) banco )
(print "================================================== ==================================================")
(newline)
(print "FINAL")
(newline)
(print "================================================== ")
(newline)