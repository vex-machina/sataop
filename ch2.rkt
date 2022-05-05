#lang racket

(define (second lst) (cadr lst))

(define (third lst) (caddr lst))

(define (juggle lst) 
    (cons (caddr lst) (cons (car lst) (list (cadr lst)))))

(define (switch lst) 
    (cons (caddr lst) (cons (cadr lst) (list(car lst)))))

(define member?
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      ((equal? item (car ls)) #t)
      (else (member? item (cdr ls))))))

(define subst-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls)) (cons new (cdr ls)))
      (else (cons (car ls) (subst-1st new old (cdr ls)))))))

(define insert-right-1st
  (lambda (ins tgt ls)
    (cond
      ((null? ls) '())
      ((equal? tgt (car ls)) (cons tgt (cons ins (cdr ls))))
      (else (cons (car ls) (insert-right-1st ins tgt (cdr ls)))))))

(define insert-left-1st
  (lambda (ins tgt ls)
    (cond
      ((null? ls) '())
      ((equal? tgt (car ls)) (cons ins (cons tgt (cdr ls))))
      (else (cons (car ls) (insert-left-1st ins tgt (cdr ls)))))))

(define list-of-first-items
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (cons (car (car ls)) (list-of-first-items (cdr ls)))))))

(define replace
  (lambda (sub ls)
    (cond
      ((null? ls) '())
      (else (cons sub (replace sub (cdr ls)))))))

(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      (else (cons (car ls) (remove-1st item (cdr ls)))))))

(define remove-2nd
  (lambda (tgt ls)
    (cond
      ((null? ls) '())
      ((equal? tgt (car ls)) (cons (car ls) (remove-1st tgt (cdr ls))))
      (else (cons (car ls) (remove-2nd tgt (cdr ls)))))))

(define remove-last
  (lambda (tgt ls)
    (cond
      ((null? ls) '())
      ((and (equal? tgt (car ls)) (equal? (member? tgt (cdr ls)) #f)) 
        (cdr ls))
      (else (cons (car ls) (remove-last tgt (cdr ls)))))))

(define sandwich-1st
  (lambda (a b ls)
    (cond
      ((null? ls) '())
      ((equal? (length ls) 1) ls)
      ((and (equal? (car ls) b) (equal? (cadr ls) b))
        (cons b (cons a (cons b (cddr ls)))))
      (else (cons (car ls) (sandwich-1st a b (cdr ls)))))))

(define (list-of-symbols? ls)
  (or (null? ls) (and (symbol? (car ls)) (list-of-symbols? (cdr ls)))))

(define all-same?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((equal? (length ls) 1) #t)
      (else (and (equal? (car ls) (cadr ls)) (all-same? (cdr ls)))))))
