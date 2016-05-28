;; The Law of Bolacha
;; -> Bolacha is always a number
(define bolacha 5)

;; The First Commandment
;; -> When recurring on a list of atoms, lat, ask two questions
;; about it: (null? lat) and else.
;; When recurring on a number, n, ask two questions about
;; it: (zero? n) and else.

;; Second Commandment
;; -> Use cons to build lists.

;; The Third Commandment
;; -> When building a list, describe the first typical element, and then
;; cons it onto the natural recursion.

;; The Fourth Commandment
;; -> Always change at least one argument while recurring. It must be
;; changed to be closer to termination. The changing argument must be
;; tested in the termination condition:
;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?

;; The Fifth Commandment
;; When building a value with plus, always use 0 for the value of the
;; termination line, for adding 0 does not change the value of an addtion.
;;
;; When building a value with times, always use 1 for the value of the
;; terminating line, for multiplying by 1 does not change the value of
;; a multiplication.
;;
;; When building a value with cons, always consider () for the value of the
;; terminating line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The proof of the pudding is in the eating. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contract: atom : s-expression  ->  boolean
;; Purpose: determine if a given s-expression is
;; an atom.
;; Example: (atom 5) should produce #t
;; Definition:
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      (else (cond
;;             ((eq? (car lat) a) (cdr lat))
;;             (else (rember a
;;                           (cdr lat))))))))

;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      (else (cond
;;             ((eq? (car lat) a) (cdr lat))
;;             (else (cons (car lat)
;;                         (rember a
;;                                 (cdr lat)))))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

;; Contract: firsts : list  ->  list
;; Purpose: to return the first element of each
;; sublist inside the given list.
;; Definition:
(define firsts
  (lambda (lol)
    (cond
     ((null? lol) (quote ()))
     ((atom? (car lol))
      (cons (car lol) (firsts (cdr lol))))
     (else (cons (car (car lol)) (first (cdr lol)))))))

;; (define firsts
;;   (lambda (lol)
;;     (cond
;;      ((null? lol) (quote ()))
;;      (else (cons (car (car lol)) (first (cdr lol)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (cons old (cdr lat))))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? a (car lat))
      (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a
                              (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;; (define plus
;;   (lambda (n1 n2)
;;     (cond
;;      ((zero? n2) n1)
;;      (else (plus (add1 n1) (sub1 n2))))))
(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (plus (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (plus n (times n (sub1 m)))))))

;; (define tup+
;;   (lambda (tup1 tup2)
;;     (cond
;;      ((and (null? tup1) (null? tup2)) (quote ()))
;;      (else
;;       (cons
;;        (plus
;;         (car tup1)
;;         (car tup2))
;;        (tup+ (cdr tup1) (cdr tup2)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (plus (car tup1) (car tup2))
            (tup+
             (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (n1 n2)
    (cond
     ((zero? n1) #f)
     ((zero? n2) #t)
     (else
      (gt (sub1 n1) (sub1 n2))))))

(define lt
  (lambda (n1 n2)
    (cond
     ((zero? n2) #f)
     ((zero? n1) #t)
     (else
      (lt (sub1 n1) (sub1 n2))))))

;; (define equalto
;;   (lambda (n m)
;;     (cond
;;      ((zero? m) (zero? n))
;;      ((zero? n) #f)
;;      (else (equalto (sub1 n) (sub1 m))))))

(define equalto
  (lambda (n m)
    (cond
     ((gt n m) #f)
     ((lt n m) #f)
     (else #t))))

(define pow
  (lambda (n e)
    (cond
     ((zero? e) 1)
     (else
      (times (pow n (sub1 e)) n)))))

(define div
  (lambda (n m)
    (cond
     ((lt n m) 0)
     (else (add1 (div (minus n m) m))))))

(define len
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (len (cdr lat)))))))

(define pick
  (lambda (c lat)
    (cond
     ((null? lat) #f)
     ((= c 1) (car lat))
     (else (pick (sub1 c) (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (c lat)
    (cond
     ((null? lat) (quote ()))
     ((one? c) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 c) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat))
      (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)
           (number? a2))
      (= a1 a2))
     ((or (number? a1)
          (number? a2))
      #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat))
      (add1
       (occur a (cdr lat))))
     (else
      (occur a (cdr lat))))))
