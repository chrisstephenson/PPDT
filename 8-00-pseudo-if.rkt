#lang lazy

(define (pseudo-if a b c)
  (cond
    (a b)
    (else c)))

(define (factorial n)
  (pseudo-if (<= n 1) 1 (* n (factorial (- n 1)))))