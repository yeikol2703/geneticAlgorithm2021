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

(define banco '(1 2 3 5 6 9 32 1 2 5 12 31 15)) ;lista de genes
(define characteristic '(30 60 90))
(define indv '(((2 6 2) 10) ((9 5 1 1 5) 21) ((3 31 15) 49) ((12 32) 44)))

;*************** Funciones ***************

; Suma la lista y da un valor
; P: L = Cualquier lista
; S: #Total
(define (sumList L)
   (if (empty? L) 0 
       (+ (car L) (sumList (cdr L))) 
   )
)

; Saca el valor optimo de la generacion
; P: g = grupos L: Lista de genes '(# # # # #...#)
; S: #valorOptimo
(define getOptimum 
  (lambda (g L)
    (/ (sumList L) g)
    )
  )

;*************** Genes ***************

; Produce una caracteristica del individuo. 
; P: L = Lista del grupo de genes de la caracteristcia
; S: '(((# # ... #) #Total)
(define (breedCharacteristic L) 
  (if (empty? L) '()
      (cons L (list (sumList L)))
   )
)

;*************** Individuo ***************

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

; Genera la lista de n grupos segun los genes de la lista
; P: L = Lista de banco de genes
; S: '((# # #) (# # #...#) (# # #) (# # #)...)
(define listIndv
  (lambda ( m L)
    (listIndv-x (shuffle L) m '())
    ))

;Genera el individuo apartir de los grupos de genes
;Utiliza la funcion *listarIndv*
;P: L = lista de genes en grupos
;S: '(((# # #) #T) ((# # #) #T) ((# # #) #T) ((# # #) #T))
(define (breedIndv L)
  (if (empty? L) '()
      (cons (breedCharacteristic (car L)) (breedIndv (cdr L)))
      )
 )

;Genera la lista de los valores de las caracteristicas del individuo
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

;sumarLista (deductOptimum banco (cadar lst) )

;(deductOptimum banco individuo)


(define (quicksort lst gen )
  (cond
    ((or (null? lst) ; empty list is sorted
         (null? (cdr lst))) ; single-element list is sorted
     lst)
    (else
      (let ((pivot (sumList (deductOptimum gen (car lst) ))) ; Select the first element as the pivot
            (rest (cdr lst)))
        (append
          (quicksort ; Recursively sort the list of smaller values
            (filter (lambda (x) (< (sumList (deductOptimum gen x )) pivot)) rest) gen) ; Select the smaller values
          (list pivot) ; Add the pivot in the middle
          (quicksort ; Recursively sort the list of larger values
            (filter (lambda (x) (>= (sumList (deductOptimum gen  x)) pivot)) rest) gen)))))) ; Select the larger and equal values



; *************** Generacion ***************

; Genera la primera generacion apartir de la lista de genes
; P: a = poblacion / l = lista de genes / g = grupos
; S: generacion de individos
(define firstGen 
  (lambda (a l g) 
    (if (zero? a) '()
        (cons
         (breedIndv (listIndv g l)) (firstGen (- a 1) l g)
        )
    )
  )
)

;(cdaar (firstGen 4 banco 4))
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