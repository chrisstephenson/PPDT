#lang racket

(define delta 0.0000000000001)
(define tolerans 0.0000000000001)

(define türev (λ (f) (λ (x) (/ (- (f (+ x delta)) (f x)) delta))))

(define tamam-mı-yap? (λ (f) (λ (t) (< (magnitude (f t)) tolerans))))

(define iyileştir-yap (λ (f f-turev) (λ (t) (- t (/ (f t) (f-turev t))))))

(define bul
  (lambda (t tamam-mı? iyileştir)
    (cond
      ((tamam-mı? t) t)
      (else (bul (iyileştir t) tamam-mı? iyileştir)))))

(define bul-kök (λ (f t) (bul t (tamam-mı-yap? f) (iyileştir-yap f (türev f)))))

(define karekök (λ (k t) (bul-kök (λ (x) (- (* x x) k)) t)))