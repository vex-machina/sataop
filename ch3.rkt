#lang racket

(define (succ n) (+ n 1))

(define (pred n) (- n 1))

(define (harmonic-sum n)
 (cond
   ((zero? n) 0)
   (else (+ (/ 1 n) (harmonic-sum (pred n))))))

(define (list-of-zeroes n)
  (cond
    ((zero? n) '())
    (else (cons 0 (list-of-zeroes (pred n))))))

(define (my-list-ref ls n)
  (cond
    ((null? ls)
      (error "list-ref: Index" n "out of range for list" ls))
    ((zero? n) (car ls))
    (else (my-list-ref (cdr ls) (pred n)))))

(define (my-sum ls)
  (cond
    ((null? ls) 0)
    (else (+ (car ls) (my-sum (cdr ls))))))

(define (pairwise-sum ls1 ls2)
  (cond
    ((and (null? ls1) (null? ls2)) '())
    (else (cons (+ (car ls1) (car ls2)) (pairwise-sum (cdr ls1) (cdr ls2))))))

(define (pairwise-prod ls1 ls2)
  (cond
    ((and (null? ls1) (null? ls2)) '())
    (else (cons (* (car ls1) (car ls2)) (pairwise-prod (cdr ls1) (cdr ls2))))))

(define (dot-product ls1 ls2)
  (my-sum (pairwise-prod ls1 ls2)))

(define (mult-by-n n ls)
  (cond
    ((null? ls) '())
    (else (cons (* n (car ls)) (mult-by-n n (cdr ls))))))

(define (index a ls)
  (cond
    ((not (member a ls)) -1)
    ((equal? (car ls) a) 0)
    (else (succ (index a (cdr ls))))))

(define (my-make-list n a)
  (cond
    ((zero? n) '())
    (else (cons a (my-make-list (pred n) a)))))

(define (count-background a ls)
  (cond
    ((null? ls) 0)
    ((equal? (car ls) a) (count-background a (cdr ls)))
    (else (succ (count-background a (cdr ls))))))

(define (list-front ls n)
  (cond
    ((> n (length ls))
      (error "Error: length of" ls "is less than" n))
    ((zero? n) '())
    (else (cons (car ls) (list-front (cdr ls) (pred n))))))

(define (my-apply f a n)
  (cond
    ((zero? n) a)
    (else (f (my-apply f a (pred n))))))

(define (wrapa a n) (my-apply list a n))

(define (multiple? m n)
  (cond
    ((and (zero? m) (zero? n)) #t)
    ((zero? n) #f)
    (else (equal? (remainder m n) 0))))

(define (sum-of-odds n)
  (cond
    ((zero? n) 0)
    (else (+ (pred (* 2 n))
             (sum-of-odds (pred n))))))

(define (n-tuple-to-integer ls)
  (cond
    ((null? ls) 
      (error "Error: bad argument () to n-tuple-to-integer"))
    ((equal? (length ls) 1) (car ls))
    (else (+ (* (car ls) (expt 10 (pred (length ls)))) 
             (n-tuple-to-integer (cdr ls))))))
