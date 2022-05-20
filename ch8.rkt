#lang racket

(define (both pred)
  (lambda (x y)
    (and (pred x) (pred y))))

(define (neither pred)
  (lambda (x y)
    ((both (negate pred)) x y)))

(define (at-least-one pred)
  (lambda (x y)
    (not ((neither pred) x y))))


