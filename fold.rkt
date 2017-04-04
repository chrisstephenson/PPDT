#lang racket
(define my-list (cons 1 (cons 2 (cons 3 empty))))
;;
;; *** Bunlar gereksiz oldu. Hepsi birer fold
;;
;;(define (is-a-list-of-numbers? l)
;;  (cond
;;    ((empty? l) true)
;;    (else (and (number? (first l)) (is-a-list-of-numbers? (rest l))))))
;;
;;(define (topla l)
;;  (cond
;;    ((empty? l) 0)
;;    (else (+ (first l) (topla (rest l))))))
;;
;;(define (çarp l)
;;  (cond
;;    ((empty? l) 1)
;;    (else (* (first l) (çarp (rest l))))))
;;
;; Fold yeterlidir...
(define (fold birleş  ev l)
  (cond
    ((empty? l) ev)
    (else (birleş (first l) (fold birleş ev (rest l))))))

(define (mapf f ls)
  (fold (λ (l r) (cons (f l) r)) empty ls))