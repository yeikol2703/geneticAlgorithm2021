#lang racket

(define (knapsack lst)

  (define half (/ (apply + lst) 2))   ; compute half of total
  (printf "list  : ~a\nhalf  : ~a\n" lst half)

  (define (combs lst1 (lst2 null))    ; compute all the combinations  
    (if (null? lst1)
        (if (null? lst2) 
            null
            (list (reverse lst2)))
        (append
         (combs (cdr lst1) lst2)      ; case 1 -> we don't carry the iten
         (combs (cdr lst1) (cons (car lst1) lst2)))))   ; case 2 -> we do

  (for/fold ((delta half) (res null)) ((c (in-list (combs lst)))) ; determine the best fit
    (let* ((sm (apply + c)) (newdelta (abs (- half sm))))
      (cond
        ((< newdelta delta) (values newdelta (list c)))
        ((= newdelta delta) (values    delta (cons c res)))
        (else               (values    delta res))))))


 (let-values (((delta res) (knapsack (cdr (range 0 24 5)))))
   (printf "result: ~a\ndelta : ~a\n" res delta))



;============================
(define resolver
  (lambda ( m L);recibe parametro grupos y la lista L
    (resolver-x L m '());llama a otra funcion y le pasa una lista L, el numero de grupos y otra lista A
    ))

(define resolver-x
  (lambda (L m A) ;lista, el numero de grupos y otra lista
    (if (null? L) A ; si la primera lista L esta vacia imprime la lista A vacia
        (resolver-x (cdr L) m (add (car L) m A)) ;llama recursivamente agarra el segundo elemento de L, numero de grupos, llama a add
        )
    ))

(define add
  (lambda (x m A) ;valor, numero de grupos y lista A
    (cond ((null? A) (list (list x))) 
          ((= (remainder x m) (remainder (caar A) m))
           (cons (cons x (car A)) (cdr A)))
          (else
           (cons (car A) (add x m (cdr A)))))
    ))

(define shuffle
  (lambda (list)
    (if (< (length list) 2) 
        list
        (let ((item (list-ref list (random (length list)))))
          (cons item (shuffle (remove item list)))))))

; 4 es cantidad de grupos '() el list inicial
(resolver 4 '(30 60 90 25 20 15 16 120 200 43 18 43 18 30 30) )

;genera lista
(define lista(list 30 60 90 25 20 15 16 120 200 43 18 43 18 30 30))

;genera 5 individuos con 4 grupos cada uno de forma aleatoria
(resolver 4 (shuffle lista)  )
(resolver 4 (shuffle lista)  )
(resolver 4 (shuffle lista)  )
(resolver 4 (shuffle lista)  )
;======================================================

;funcion que suma todos los elementos de una lista
;suma los elementos de la lista
(define (suma lista)
 (if (empty? lista)
     0
    (+ (car lista) (suma (cdr lista)))
    )
  )
;llama la funcion
(suma '(30 60 90 25 20 15 16 120 200 43 18 43 18 30 30))


;funcion para recorrer y evaluar cada elemento en la lista

(define (recorrer lista)
(for-each (lambda (x)
            (display x))
          (list lista)
          )
  )

(recorrer lista)

;funcion para saber el length de una lista
( define ( longitud L )
 ( cond
 [( empty? L ) 0]
 [ else (+ 1 ( longitud ( rest L ) ) ) ]) )

(longitud lista)

;

(define a(list 1 2 3 4 2))
(define b(list 1 2 3 4 3))
(define c(list 1 2 3 4 4))


(define listota(list a))

(define attach-at-end
  (lambda (val li)
    (if (null? li)
        (list val)
        (cons (car li) (attach-at-end val (cdr li)))

        )))

(longitud listota)


(attach-at-end lista listota)

(recorrer (attach-at-end lista listota))






