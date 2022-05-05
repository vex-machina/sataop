#lang racket

(define (insert-left new old ls)
  (cond
    ((null? ls) '())
    ((equal? (car ls) old)
      (cons new (cons old (insert-left new old (cdr ls)))))
    (else (cons (car ls) (insert-left new old (cdr ls))))))

(define (insert-right new old ls)
  (cond
    ((null? ls) '())
    ((equal? (car ls) old)
      (cons old (cons new (insert-right new old (cdr ls)))))
    (else (cons (car ls) (insert-right new old (cdr ls))))))

(define (subst new old ls)
  (cond
    ((null? ls) '())
    ((equal? (car ls) old)
      (cons new (subst new old (cdr ls))))
    (else (cons (car ls) (subst new old (cdr ls))))))

(define (deepen-1 ls)
  (cond
    ((null? ls) '())
    (else (cons (list (car ls)) (deepen-1 (cdr ls))))))

(define (subst-all new old ls)
  (cond
    ((null? ls) '())
    ((equal? (car ls) old) 
      (cons new (subst-all new old (cdr ls))))
    ((pair? (car ls)) (cons (subst-all new old (car ls))
                            (subst-all new old (cdr ls))))
    (else (cons (car ls) (subst-all new old (cdr ls))))))

(define (insert-left-all new old ls)
  (cond
    ((null? ls) '())
    ((equal? (car ls) old) 
      (cons new (cons old (insert-left-all new old (cdr ls)))))
    ((pair? (car ls)) (cons (insert-left-all new old (car ls))
                            (insert-left-all new old (cdr ls))))
    (else (cons (car ls) (insert-left-all new old (cdr ls))))))

(define (sum-all ls)
  (cond
    ((null? ls) 0)
    ((pair? (car ls)) (+ (sum-all (car ls))
                         (sum-all (cdr ls))))
    (else (+ (car ls) (sum-all (cdr ls))))))

(define (count-parens-all ls)
  (cond
    ((null? ls) 2)
    ((pair? (car ls)) (+ (count-parens-all (car ls))
                         (count-parens-all (cdr ls))))
    ((null? (car ls)) (+ 2 (count-parens-all (cdr ls))))
    (else (count-parens-all (cdr ls)))))

(define (count-background-all item ls)
  (cond
    ((null? ls) 0)
    ((pair? (car ls)) (+ (count-background-all item (car ls))
                         (count-background-all item (cdr ls))))
    ((not (equal? (car ls) item))
      (+ 1 (count-background-all item (cdr ls))))
    (else (count-background-all item (cdr ls)))))

(define (leftmost ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (leftmost (car ls)))
    (else (car ls))))

(define depth
  (lambda (ls)
    (cond
      ((pair? ls)
       (max (add1 (depth (car ls)))
            (depth (cdr ls))))
      (else
       0))))

(define flatten
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (append (flatten (car ls))
                                (flatten (cdr ls))))
      (else
       (cons (car ls) (flatten (cdr ls)))))))

(define (rightmost ls)
  (cond
    ((null? ls) '())
    ((> (depth ls) 1) (rightmost (flatten ls)))
    ((equal? (length ls) 1) (car ls))
    (else (rightmost (cdr ls)))))

(define (harmonic-aux n acc)
  (cond
    ((zero? n) acc)
    (else (harmonic-aux (- n 1) (+ (/ 1 n) acc)))))

(define (harmonic-sum-it n)
 (harmonic-aux n 0))

(define (fibonacci-aux n acc1 acc2)
  (cond
    ((zero? n) acc1)
    (else (fibonacci-aux (sub1 n) acc2 (+ acc1 acc2)))))

(define (fibonacci-it n)
 (fibonacci-aux n 0 1))

(define (reverse-aux ls acc)
  (cond
    ((null? ls) acc)
    (else (reverse-aux (cdr ls) (cons (car ls) acc)))))

(define (reverse-it ls)
 (reverse-aux ls '()))

(define (length-aux ls acc)
  (cond
    ((null? ls) acc)
    (else (length-aux (cdr ls) (+ 1 acc)))))

(define (length-it ls)
 (length-aux ls 0))

(define (asc-aux ls acc)
  (cond
    ((null? ls) acc)
    (else (asc-aux (remove (apply max ls) ls) (cons (apply max ls) acc)))))

(define (asc-it ls)
 (asc-aux ls '()))

(define (desc-aux ls acc)
  (cond
    ((null? ls) acc)
    (else (desc-aux (remove (apply min ls) ls) (cons (apply min ls) acc)))))

(define (desc-it ls)
 (desc-aux ls '()))

(define (occurs item ls)
  (cond
    ((null? ls) 0)
    ((equal? (car ls) item) (+ 1 (occurs item (cdr ls))))
    (else (occurs item (cdr ls)))))

(define (occurs-aux item ls acc)
  (cond
    ((null? ls) acc)
    ((equal? (car ls) item) (occurs-aux item (cdr ls) (+ 1 acc)))
    (else (occurs-aux item (cdr ls) acc))))

(define (occurs-it item ls)
 (occurs-aux item ls 0))
