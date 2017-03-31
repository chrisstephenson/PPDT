#lang racket

(define karekök-tamam-mı? (λ (k) (λ (t) (< (magnitude (- (* t t) k)) 0.000000000001))))

(define karekök-iyileştir (λ (k) (λ (t) (/ (+ t (/ k t)) 2))))

(define bul
  (lambda (t tamam-mı? iyileştir)
    (cond
      ((tamam-mı? t) t)
      (else (bul (iyileştir t) tamam-mı? iyileştir)))))

