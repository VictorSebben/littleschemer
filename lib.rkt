;; The Law of Bolacha
;; -> Bolacha is always a number
(define *bolacha* 5)

;; The First Commandment
;; -> When recurring on a list of atoms, lat, ask two questions
;; about it: (null? lat) and else.
;; When recurring on a number, n, ask two questions about
;; it: (zero? n) and else.
;; When recurring on a list of S-expressions, l, ask three questions
;; about it: (null? l), (atom? (car l)), and else.

;; Second Commandment
;; -> Use cons to build lists.

;; The Third Commandment
;; -> When building a list, describe the first typical element, and then
;; cons it onto the natural recursion.

;; The Fourth Commandment
;; -> Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use (cdr lat). When
;; recurring on a number, n, use (sub1 n). And when recurring on
;; a list of S-expressions, l, use (car l) and (cdr l) if neither
;; (null? l) nor (atom? (car l)) are true.
;;
;; It must be changed to be closer to termination.
;; The changing argument must be tested in the termination condition:
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

;; The Sixth Commandment
;; Simplify only after the function is correct.

;; The Seventh Commandment
;; Recur on the subparts that are of the same nature:
;; - On the sublists of a list.
;; - On the subexpressions of an arithmetic expression.

;; The Eighth Commandment
;; Use help function to abstract from representations.

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

;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      ((eq? (car lat) a) (cdr lat))
;;      (else (cons (car lat)
;;                  (rember a (cdr lat)))))))

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

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
     ((eq? a (car l)) (rember* a (cdr l)))
     (else (cons (car l)
                 (rember* a (cdr l)))))))


(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((list? (car l))
      (cons (insertR* new old (car l))
            (insertR* new old (cdr l))))
     ((eq? old (car l))
      (cons (car l)
            (cons new (insertR* new old (cdr l)))))
     (else
      (cons (car l)
            (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((list? (car l)) (plus (occur* a (car l)) (occur* a (cdr l))))
     ((eq? a (car l)) (add1 (occur* a (cdr l))))
     (else (occur* a (cdr l))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
     ((eq? old (car l)) (cons new (subst* new old (cdr l))))
     (else (cons (car l) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((list? (car l))
      (cons (insertL* new old (car l))
            (insertL* new old (cdr l))))
     ((eq? old (car l))
      (cons new
            (cons (car l) (insertL* new old (cdr l)))))
     (else
      (cons (car l)
            (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((list? (car l)) (or (member* a (car l)) (member* a (cdr l))))
     (else (or (eq? (car l) a)
               (member* a (cdr l)))))))

(define lefmost
  (lambda (l)
    (cond
     ((null? l) #f)
     ((list? (car l)) (lefmost (car l)))
     (else (car l)))))

;; Little Schemer version, with the (null?) test already added
(define lm
  (lambda (l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (car l))
     (else (lm (car l))))))

;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;      ((and (null? l1) (null? l2)) #t)
;;      ((or (null? l1) (null? l2)) #f)
;;      ((and (list? (car l1)) (list? (car l2)))
;;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;;      ((eqan? (car l1) (car l2))
;;       (and (eqlist? (cdr l1) (cdr l2))))
;;      (else #f))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (list? s1) (list? s2)) (eqlist? s1 s2))
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2 ))
     (else #f))))

(define eqlist
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))

;; (define numbered?
;;   (lambda (a)
;;     (cond
;;      ((null? a) #t)
;;      ((list? (car a)) (and (numbered? (car a)) (numbered? (cdr a))))
;;      ((and (number? (car a)) (numbered? (cdr a))))
;;      ((and (member? (car a) (quote (+ * ^))) (numbered? (cdr a))))
;;      (else #f))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))
           (member? (car (cdr aexp)) (quote (+ * ^))))))))

;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp)
;;       (cond ((number? nexp) nexp) (else #f)))
;;      ((eq? (car (cdr nexp)) (quote +))
;;       (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
;;      ((eq? (car (cdr nexp)) (quote *))
;;       (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
;;      ((eq? (car (cdr nexp)) (quote ^))
;;       (pow (value (car nexp)) (value (car (cdr (cdr nexp))))))
;;      (else #f))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp)
      (cond ((number? nexp) nexp) (else #f)))
     ((eq? (operator nexp) (quote +))
      (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *))
      (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote ^))
      (pow (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     (else #f))))

(define 1st-sub-exp
  (lambda (nexp)
    (car nexp)))

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define operator
  (lambda (nexp)
    (car (cdr nexp))))

(define crazy-zero?
  (lambda (cn)
    (null? cn)))

(define crazy-add1
  (lambda (cn)
    (cons (quote ()) cn)))

(define crazy-sub1
  (lambda (cn)
    (cdr cn)))

(define crazy+
  (lambda (cn1 cn2)
    (cond
     ((crazy-zero? cn2) cn1)
     (else (crazy+ (crazy-add1 cn1) (crazy-sub1 cn2))))))

(define crazy-lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((empty? (car l)) (crazy-lat? (cdr l)))
     (else #f))))

;; ((()) (() ()) (() () ())) => (1 2 3) in crazy-numeric notation
