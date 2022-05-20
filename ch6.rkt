#lang racket

(define (substring? sub str)
  (letrec ((subl (string-length sub))
          (strl (string-length str))
          (diff (- strl subl))
          (fst-char (lambda (s) (substring s 0 1))))
    (cond
      ((string=? sub str) #t)
      ((string=? sub "") #t)
      ((< strl subl) #f)
      ((equal? (fst-char sub) (fst-char str)) 
        (or (substring? sub (substring str 0 diff))
            (substring? sub (substring str 1 strl))))
      (else
        (substring? sub (substring str 1 strl))))))

(define (substring-ref strng n)
  (substring strng n (add1 n)))

(define (reverse-string str)
  (letrec ((aux
             (lambda (s acc)
               (cond
                 ((string=? s "") acc)
                 (else 
                   (aux (substring str 0 (sub1 (string-length s))) 
                        (string-append acc 
                                (substring-ref s (sub1 (string-length s))))))))))
    (aux str "")))

(define (palindrome? str)
  (equal? str (reverse-string str)))

(define i-sqrt
  (lambda ()
    (printf "Enter the number whose square root you want, or enter stop to quit: ")
    (let ((n (read)))
      (cond 
        ((eq? n 'stop)
          (printf "That's all, folks.\n"))
        (else
          (begin
            (printf "The square root of ~a is ~a\n" n (sqrt n))
            (newline)
            (i-sqrt)))))))
