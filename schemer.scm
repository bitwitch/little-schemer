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
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

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


































