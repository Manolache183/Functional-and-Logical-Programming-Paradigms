#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

;; 2 helpere facute de mine pentru conversie
(define (uncurry->curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (curry->uncurry f)
  (lambda (x y)
    ((f x) y)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (foldl (lambda (x y result) (+ result (* x y))) 0 X Y))


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
  (map ((uncurry->curry dot-product) V) M))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
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
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).

; aplic functia fiecare functie f din Fs pe un acumulator ca initial e tuple
(define (apply-functional-transformations Fs tuple)
  (foldl (lambda (f acc) (f acc)) tuple Fs))


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.

; 'A' e pus in lambda doar sa fie functia curry
; cu 'foldl' fac ca pentru fiecare 'a' din 'A' sa ii aplic acumulatorului (initial 'tuple') functia aferenta din Fs
(define (get-nth-tuple n tuple Fs)
  (lambda (A)
    (foldl (lambda (a acc)
             (cond
               ((= a 1) ((car Fs) acc))
               ((= a 2) ((cadr Fs) acc))
               (else ((caddr Fs) acc)))) tuple A)))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.

; tot ce trebuie facut e sa facem functia 'multiply' curry, deoarece trebuie sa primeasca parametrii pe rand
(define (get-nth-ppt-from-matrix-transformations n)
  ((get-nth-tuple n '(3 4 5) (list ((uncurry->curry multiply) T1) ((uncurry->curry multiply) T2) ((uncurry->curry multiply) T3))) (get-transformations n)))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.

; tot ce trebuie facut e sa facem functia 'apply' curry, deoarece trebuie sa primeasca parametrii pe rand
(define (get-nth-quadruple n)
  ((get-nth-tuple n '(1 1 2 3) (list ((uncurry->curry apply) Q1) ((uncurry->curry apply) Q2) ((uncurry->curry apply) Q3))) (get-transformations n)))


;(ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-nth-ppt-from-GH-quadruples n)
  (helper-nth-ppt-from-GH-quadruples (get-nth-quadruple n)))

(define (helper-nth-ppt-from-GH-quadruples L)
  (list (* (car L) (cadddr L)) (* 2 (* (cadr L) (caddr L))) (+ (* (cadr L) (cadr L)) (* (caddr L) (caddr L))) ))
