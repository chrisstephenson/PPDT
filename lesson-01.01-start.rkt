#lang racket

(define karekök
  (lambda (t k)
    (cond
      ((< (magnitude (- (* t t) k)) 0.000000000001) t)
      (else (karekök (/ (+ t (/ k t)) 2) k)))))

(define bul
  (lambda (t tamam-mı? iyileştir)
    (cond
      ((tamam-mı? t) t)
      (else (bul (iyileştir t) tamam-mı? iyileştir)))))

