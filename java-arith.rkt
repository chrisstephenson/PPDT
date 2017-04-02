#lang racket
(define (integer-to-java-int n)
  (let ((nmod-32 (modulo n (expt 2 32)))) 
  (cond
    ((<  nmod-32 (expt 2 31)) nmod-32)
    (else (- nmod-32 (expt 2 32))))))

;(define (java-int-to-integer 