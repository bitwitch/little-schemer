;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                              1. Toys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define atom?
   (lambda (x)
     (and (not (pair? x)) (not (null? x)))))

(define add1 (lambda (n) (+ n 1)))

(define sub1 (lambda (n) (- n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               2. Do it, do it again, and again, and again... 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         3. Cons the Magnificent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (lol)
    (cond
      ((null? lol) '())
      (else (cons (car (car lol)) 
                  (firsts (cdr lol)))))))

(define insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old) 
            (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
            (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
            (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
            (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiInsertR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old) 
            (cons (car lat) (cons new (multiInsertR new old (cdr lat)))))
      (else (cons (car lat) (multiInsertR new old (cdr lat)))))))

(define multiInsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
            (cons new (cons (car lat) (multiInsertL new old (cdr lat)))))
      (else (cons (car lat) (multiInsertL new old (cdr lat)))))))

(define multiSubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
            (cons new (multiSubst new old (cdr lat))))
      (else (cons (car lat) (multiSubst new old (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         4. Numbers Games
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define add
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (add (add1 n) (sub1 m))))))

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
   (cond
     ((null? tup) 0)
     (else (add (car tup) (addtup (cdr tup)))))))

(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m)))))))

(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (add (car t1) (car t2))
                  (tup+ (cdr t1) (cdr t2)))))))






















