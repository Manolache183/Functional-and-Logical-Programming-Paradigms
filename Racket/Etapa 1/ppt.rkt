#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y))))



   )


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (helper-multiply M V null))

(define (helper-multiply M V res)
  (if (null? M)
      res
      (helper-multiply (cdr M) V (append res (list (dot-product (car M) V)))))

  
  )


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
(define (get-transformations n)
  (helper-get-transformations n (car (get-min-max-level n 1 0)) (cadr (get-min-max-level n 1 0)) '()))


;; min max pentru un nivel
(define (get-min-max-level n sum pow)
  (if (<= n sum)
      (append (list (+ (- sum (expt 3 pow)) 1)) (list sum))
      (get-min-max-level n (+ sum (expt 3 (+ pow 1))) (+ pow 1))))
  

;; daca n e mai mic decat min + prima treime (daca ai 9 el ar trebui sa fie min + 3), se schimba max la min + 3 - 1 samd
(define (helper-get-transformations n min max res)
  (cond
    ((= min max) res)
    ((< n (+ min (quotient (+ (- max min) 1) 3))) (helper-get-transformations n min (+ min (- (quotient (+ (- max min) 1) 3) 1)) (append res '(1))))
    ((< n (+ min (* (quotient (+ (- max min) 1) 3) 2))) (helper-get-transformations n (+ min (quotient (+ (- max min) 1) 3)) (+ min (- (* (quotient (+ (- max min) 1) 3) 2) 1)) (append res '(2))))
    (else (helper-get-transformations n (+ min (* (quotient (+ (- max min) 1) 3) 2)) max (append res '(3)))))  
  
  )
  


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (cond
    ((null? Ts) ppt)
    ((= (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
    ((= (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
    ((= (car Ts) 3) (apply-matrix-transformations (cdr Ts) (multiply T3 ppt)))
    )



    )


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))







