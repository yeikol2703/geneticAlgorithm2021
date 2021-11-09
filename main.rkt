#lang racket

;*************** Datos de prueba ***************

(define banco '(1 2 3 5 6 9 32 1 2 5 12 31 15)) ;lista de genes
(define caracteristica '(30 60 90))
(define individuo '(((2 6 2) 10) ((9 5 1 1 5) 21) ((3 31 15) 49) ((12 32) 44)))

;*************** Funciones ***************

; Sumna la lista y da un valor
; P: L = Cualquier lista
; S: #Total
(define (sumarLista L)
   (if (empty? L) 0 
       (+ (car L) (sumarLista (cdr L))) 
   )
)

; Saca el valor optimo de la generacion
; P: g = grupos L: Lista de genes '(# # # # #...#)
; S: #valorOptimo
(define valorOptimo 
  (lambda (g L)
    (/ (sumarLista L) g)
    )
  )

;*************** Genes ***************

; Produce una caracteristica del individuo. 
; P: L = Lista del grupo de genes de la caracteristcia
; S: '(((# # ... #) #Total)
(define (generarCaracteristica L) 
  (if (empty? L) '()
      (cons L (list (sumarLista L)))
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

(define listarIndv-x
  (lambda (L m A)
    (if (null? L) A
        (listarIndv-x (cdr L) m (add (car L) m A)))
    ))

; Genera la lista de n grupos segun los genes de la lista
; P: L = Lista de banco de genes
; S: '((# # #) (# # #...#) (# # #) (# # #)...)
(define listarIndv
  (lambda ( m L)
    (listarIndv-x (shuffle L) m '())
    ))

;Genera el individuo apartir de los grupos de genes
;Utiliza la funcion *listarIndv*
;P: L = lista de genes en grupos
;S: '(((# # #) #T) ((# # #) #T) ((# # #) #T) ((# # #) #T))
(define (generarIndiv L)
  (if (empty? L) '()
      (cons (generarCaracteristica (car L)) (generarIndiv (cdr L)))
      )
 )

;Genera la lista de los valores de las caracteristicas del individuo
(define (valoresCaractIndv L)
  (if (empty? L)'()
      (cons (cadar L) (valoresCaractIndv (cdr L)))
      )
 )


(define restarOptimo
  (lambda (L c)
    (map (lambda (x) (abs(- (valorOptimo 4 L) x ))) (valoresCaractIndv c)   )
    )
 )

;sumarLista (restarOptimo banco (cadar lst) )

;(restarOptimo banco individuo)


(define (quicksort lst gen )
  (cond
    ((or (null? lst) ; empty list is sorted
         (null? (cdr lst))) ; single-element list is sorted
     lst)
    (else
      (let ((pivot (sumarLista (restarOptimo gen (car lst) ))) ; Select the first element as the pivot
            (rest (cdr lst)))
        (append
          (quicksort ; Recursively sort the list of smaller values
            (filter (lambda (x) (< (sumarLista (restarOptimo gen x )) pivot)) rest) gen) ; Select the smaller values
          (list pivot) ; Add the pivot in the middle
          (quicksort ; Recursively sort the list of larger values
            (filter (lambda (x) (>= (sumarLista (restarOptimo gen  x)) pivot)) rest) gen)))))) ; Select the larger and equal values



; *************** Generacion ***************

; Genera la primera generacion apartir de la lista de genes
; P: a = poblacion / l = lista de genes / g = grupos
; S: generacion de individos
(define primeraGeneracion 
  (lambda (a l g) 
    (if (zero? a) '()
        (cons
         (generarIndiv (listarIndv g l)) (primeraGeneracion (- a 1) l g)
        )
    )
  )
)

;(cdaar (primeraGeneracion 4 banco 4))
(print "INICIO")
(newline)
(print "================================================== ==================================================")
(newline)
(print "GENERACION INICIAL")
(newline)
(print "====================================================================================================")
(newline)
(primeraGeneracion 4 banco 4)
(print "====================================================================================================")
(newline)
(print "Individuo  optimo es: ")
(newline)
(quicksort (primeraGeneracion 4 banco 4) banco )
(print "================================================== ==================================================")
(newline)
(print "FINAL")
(newline)
(print "================================================== ")
(newline)