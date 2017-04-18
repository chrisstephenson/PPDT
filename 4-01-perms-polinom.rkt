#lang racket

(define insert-everywhere
  (λ (x ls)
    (cond
      ((empty? ls) (list (list x)))
      (else (cons (cons x ls) (map (curry cons (first ls)) (insert-everywhere x (rest ls))))))))

(define perms
  (λ (ls)
    (foldr (λ (l r) (foldr append empty (map (curry insert-everywhere l) r))) '(()) ls)))

(define sıfırlar
  (λ (n)
    (cond
      ((<= n 0) empty)
      (else (cons 0 (sıfırlar (- n 1)))))))

(define pad-polinom
  (λ (p uzunluk)
    (append p (sıfırlar (- uzunluk (length p))))))

(define topla-polinom
  (λ (p1 p2)
    (map + (pad-polinom p1 (length p2))
         (pad-polinom p2 (length p1)))))

(define çarp-polinom-tek-değer (λ (n p) (map (curry * n) p)))

(define çarp-polinom
  (λ (p1 p2)
    (foldr (λ (l r) (topla-polinom (çarp-polinom-tek-değer l p1) (cons 0 r))) empty p2)))

(define kökler->polinom
  (λ (kl)
    (foldr çarp-polinom '(1) (map (λ (k) (list k -1)) kl))))