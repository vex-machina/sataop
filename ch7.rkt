#lang racket

(define (compose f g) 
  (lambda (x) (f (g x))))

(define (compose3 f g h)
  (lambda (x)
    (f (g (h x)))))

(define compose-many
  (lambda args
    (cond
      ((null? args)
        (lambda (x) x))
      (else
        (compose (car args) (apply compose-many (cdr args)))))))

(define (subtract x y)
	(cond 
      ((zero? y) x)
	  (else (sub1 (subtract x (sub1 y))))))

(define (map-first-two fun ls)
  (letrec ((aux
            (lambda (fun ls acc)
              (cond
                ((null? (cdr ls)) acc)
                (else
                  (aux fun (cdr ls) (append acc (list (fun (car ls) (cadr ls))))))))))
    (aux fun ls '())))

(define (my-reduce fun ls)
  (cond
    ((null? ls) '())
    ((equal? (length ls) 2) (fun (car ls) (cadr ls)))
    (else (my-reduce fun (cons (fun (car ls) (cadr ls)) (cddr ls))))))

(define (andmap pred ls)
  (foldr (lambda (x acc) (and acc (pred x))) #t ls))

(define (map2 fun ls1 ls2)
  (cond
    ((or (null? ls1) (null? ls2)) '())
    (else (cons (fun (car ls1) (car ls2))
                (map2 fun (cdr ls1) (cdr ls2))))))

(define (map2-alt fun ls1 ls2)
  (foldr (lambda (x acc) (foldr (lambda (y bcc) (cons (+ x y) bcc)) acc ls2)) '() ls1))

(define (curried* m)
  (lambda (n)
    (* m n)))

(define (times10 n)
  ((curried* 10) n))

(define (swapper-c x)
  (lambda (y)
    (lambda (ls)
      (foldr
        (lambda (i acc)
          (cond
            ((equal? i x) (cons y acc))
            ((equal? i y) (cons x acc))
            (else (cons i acc))))
        '()
        ls))))

(define (subst-all-m new old)
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) old) (cons new ((subst-all-m new old) (cdr ls))))
      ((pair? (car ls)) (cons ((subst-all-m new old) (car ls)) 
                              ((subst-all-m new old) (cdr ls))))
      (else (cons (car ls) ((subst-all-m new old) (cdr ls)))))))

(define (extreme-value-c pred)
  (lambda (x y)
    (cond
      ((pred x y) x)
      (else y))))

(define (rmax x y)
  ((extreme-value-c >) x y))

(define (rmin x y)
  ((extreme-value-c <) x y))

(define (foldr1 f xs)
  (foldr f (car xs) (cdr xs)))

(define (extreme-value-alt pred)
  (lambda args
    (foldr1
      (lambda (x acc)
        (cond
          ((pred x acc) x)
          (else acc))) 
      args)))

(define (between? x y z)
  (and (< x y) (< y z)))

(define (between?-c x)
  (lambda (y)
    (lambda (z)
      (and (< x y) (< y z)))))

(define (mult-by-scalar n)
  (curry map (curry * n)))

(define (filter-out pred ls)
  (filter (negate pred) ls))

(define (partial-sum fun m n)
  (foldr + 0 (map fun (inclusive-range m n))))

(define (partial-product fun m n)
  (foldr * 1 (map fun (inclusive-range m n))))

(define (partial acc f)
  (lambda (fun m n)
    (foldr f acc (map fun (inclusive-range m n)))))

(define (remove-all-c-alt item ls)
  (foldr
    (lambda (x acc)
      (cond
        ((pair? x) (cons 
                     (remove-all-c-alt item x)
                     acc))
        ((equal? x item) acc)
        (else (cons x acc))))
    '()
    ls))

(define (filter-out-all pred ls)
  (foldr
    (lambda (x acc)
      (cond
        ((pair? x) (cons
                     (filter-out-all pred x)
                     acc))
        ((pred x) acc)
        (else (cons x acc))))
    '()
    ls))

(define (subst-all-m-alt new old ls)
  (foldr
    (lambda (x acc)
      (cond
        ((pair? x) (cons
                     (subst-all-m-alt new old x)
                     acc))
        ((equal? x old) (cons new acc))
        (else (cons x acc))))
    '()
    ls))

(define (sum-all ls)
  (foldr
    (lambda (x acc)
      (cond
        ((pair? x) (+
                     (sum-all x)
                     acc))
        (else (+ x acc))))
    0
    ls))

(define (reverse-all ls)
  (foldl
    (lambda (x acc)
      (cond
        ((pair? x) (cons
                     (reverse-all x)
                     acc))
        (else (cons x acc))))
    '()
    ls))
