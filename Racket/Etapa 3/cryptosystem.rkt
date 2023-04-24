#lang racket

(require "ppt.rkt")

(provide (all-defined-out))

;; Pornind de la numerele e și f din cvartetele GH, putem
;; obține trei soluții (a1,b1,c1), (a2,b2,c2), (a3,b3,c3)
;; care respectă relația a^2 + b^2 = c^2.
;; Aceste triplete nu reprezintă TPP, și în anumite cazuri
;; conțin coeficienți negativi, însă acest lucru nu ne
;; împiedică să le folosim pentru a crea cheia de
;; criptare/decriptare pentru un criptosistem simplu.

;; Cele trei soluții se obțin conform formulelor de mai jos:
(define (a1 e f) (+ (* e e) (* 2 f e)))
(define (b1 e f) (+ (* 2 f f) (* 2 f e)))
(define (c1 e f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 e f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 e f) (* 2 f e))
(define (c3 e f) (+ (* f f) (* e e)))

;lista cu functiile
(define abc_f_list (list a1 b1 c1 a2 b2 c2 a3 b3 c3))

;; Funcția check nu are utilitate pentru temă, este doar
;; un mod de a:
;; - vizualiza tripletele generate pentru 2 numere e și f
;; - testa că ele respectă relația a^2 + b^2 = c^2
(define (check e f)
  (let ((a1 (a1 e f)) (b1 (b1 e f)) (c1 (c1 e f))
                      (a2 (a2 e f)) (b2 (b2 e f)) (c2 (c2 e f))
                      (a3 (a3 e f)) (b3 (b3 e f)) (c3 (c3 e f)))
    (display (list (list a1 b1 c1) (list a2 b2 c2) (list a3 b3 c3)))
    (newline)
    (and
     (= (+ (sqr a1) (sqr b1)) (sqr c1))
     (= (+ (sqr a2) (sqr b2)) (sqr c2))
     (= (+ (sqr a3) (sqr b3)) (sqr c3)))))

;; Exemplu de utilizare
;; (cu primele 3 generații din arborele de cvartete):
;; (map check
;;      '(1 1 2 2 1 4 4 2 5 5 2 3 3)
;;      '(2 4 5 3 6 9 7 9 12 8 7 8 4))

;; Vom trimite mesaje folosind cele 26 de caractere din
;; alfabetul englez (doar în varianta cu literă mică),
;; plus caracterul "spațiu". În consecință, avem nevoie să
;; codificăm 27 de caractere distincte:
;;  - asociem codul 0 caracterului "spațiu"
;;  - asociem codurile 1-26 caracterelor 'a', 'b' ... 'z'
;;
;; Cele două părți angrenate în comunicare își vor trimite un
;; număr n, pe baza căruia fiecare va determina cheia de
;; criptare/decriptare astfel:
;;  - determină al n-lea cvartet din arborele TPP
;;  - extrag valorile e și f din acest cvartet
;;  - cheia va fi (a1,b1,c1,a2,b2,c2,a3,b3,c3) mod 27
;;    (fiecare valoare din tuplu devine un cod între 0 și 26)


; TODO
; Implementați o funcție care primește un număr n și
; obține cheia de criptare/decriptare conform algoritmului
; de mai sus.
; Folosiți o formă adecvată de let pentru a extrage e și f.
; Utilizați o funcțională astfel încât să nu scrieți 9 bucăți 
; de cod foarte similare (de exemplu, numele operației mod ar
; trebui să apară o singură dată).

; e = elem 2
; f = elem 3

; salvez in gefh_list al n ulea cvartet, din care extrag ulterior e si f
; let* pentru ca folosesc gefh_list trebuie sa fie vizibil la celelalte argumente
; aplic cu map fiecare functie din abc_f_list pe e si f si fac restul cu 27
; abc_f_list e declarat mai sus cu lista functiilor a1 b1 c1 a2 b2 c2 a3 b3 c3

(define (key n)
  (let* ([gefh_list (get-nth-quadruple n)] [e (cadr gefh_list)] [f (caddr gefh_list)])
    (map (λ (func) (modulo (func e f) 27)) abc_f_list)))


; TODO
; Implementați o funcție care primește un mesaj (un șir de
; caractere care conține doar litere mici și spații) și
; întoarce o listă de coduri asociate mesajului respectiv
; (spațiu devine 0, 'a' devine 1 ... 'z' devine 26).
; Funcții utile: string->list, char->integer


; let ca sa apelez functia string->list o singura data, salvand lista de litere
; urmatorul let e ca sa apelez char->integer o singura data, pentru a-l folosi in comparatii
; daca avem caracterul 32 => spatiu => 0
; altfel aducem din intervalul 97 - 122 la 1 - 26
; map trece prin fiecare litera din mesaj

(define (message->codes message)
    (map (λ (chr)
           (let ([nr (char->integer chr)])
             (if (= nr 32)
                 0
                 (- nr 96)))) (string->list message)))
                 


; TODO
; Implementați o funcție care primește o listă de coduri
; (numere între 0 și 26) și întoarce mesajul asociat
; (procesul invers celui de la funcția message->codes).
; Funcții utile: list->string, integer->char

; trecem prin fiecare nr si i asociem litera bla bla ca la functia trecuta
(define (codes->message codes)
  (list->string
   (map (λ (nr)
         (let ([chr (integer->char (+ nr 96))])
           (if (= nr 0)
               (integer->char 32)
               chr))) codes)))
               


;; Pentru operațiile de criptare și decriptare, lucrăm
;; cu coduri între 0 și 26.
;; Folosim următoarele notații:
;;    m = un cod din reprezentarea mesajului original
;;    c = un cod din reprezentarea mesajului criptat
;;    k = un cod din reprezentarea cheii
;;
;; Pentru a putea efectua operații de criptare/decriptare,
;; cheia trebuie extinsă/trunchiată la dimensiunea
;; mesajului care trebuie criptat/decriptat.
;;
;; Mai departe, criptarea se realizează conform formulei:
;;   c = (m + k) mod 27   (pentru fiecare index)          
;;
;; Similar, formula pentru decriptare este:
;;   m = (c - k) mod 27   (pentru fiecare index)


; TODO
; Implementați o funcție care primește o cheie key și o
; dimensiune size și extinde/trunchiază cheia key la
; dimensiunea size.
; Folosiți cel puțin o formă de let.

; am evitat sa apelez length facand un let cu key_size
; daca are size ul bun intoarce cheia
; daca are size ul mai mic dubleaza cheia
; daca are size ul mai mare taie ultimul element

(define (extend-key key size)
  (let ([key_size (length key)])
    (cond
      ((= key_size size) key)
      ((< key_size size) (extend-key (append key key) size))
      (else (extend-key (reverse (cdr (reverse key))) size)))))


; TODO
; Observați că algoritmii de criptare/decriptare se
; aseamănă foarte mult. Abstractizați procesul într-o
; funcție mai generală (încercați să îi dați un nume
; sugestiv) din care derivați apoi, cu minim de efort,
; funcțiile:
;    (encrypt-codes message key)
;    (decrypt-codes crypted key)
; Nu folosiți recursivitate explicită (ci funcționale).

; Ambele funcții primesc două liste de coduri (reprezentând
; mesajul clar/criptat, respectiv cheia) și întorc o listă
; de coduri (mesajul criptat/decriptat, după caz).

;; Pentru operațiile de criptare și decriptare, lucrăm
;; cu coduri între 0 și 26.
;; Folosim următoarele notații:
;;    m = un cod din reprezentarea mesajului original
;;    c = un cod din reprezentarea mesajului criptat
;;    k = un cod din reprezentarea cheii

;; Mai departe, criptarea se realizează conform formulei:
;;   c = (m + k) mod 27   (pentru fiecare index)          
;;
;; Similar, formula pentru decriptare este:
;;   m = (c - k) mod 27   (pentru fiecare index)

(define (helper-codes func code key)
  (map (λ (a b) (modulo (func a b) 27)) code (extend-key key (length code))))

(define (encrypt-codes code key)
  (helper-codes + code key))

(define (decrypt-codes code key)
  (helper-codes - code key))


; TODO
; Analog funcțiilor encrypt-codes și decrypt-codes care
; operează pe coduri, implementați funcțiile encrypt-message
; și decrypt-message care operează pe șiruri de caractere.
; În acest caz, ambele funcții primesc un șir de caractere
; (mesajul clar/criptat) și o listă de coduri (cheia) și
; întorc un șir de caractere (mesajul criptat/decriptat).

; mesaj -> cod -> encrypt -> mesaj

(define (encrypt-message message key)
  (codes->message (encrypt-codes (message->codes message) key)))
(define (decrypt-message message key)
  (codes->message (decrypt-codes (message->codes message) key)))
           