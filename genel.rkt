#lang racket
; sayısal türev için delta değeri
(define delta 0.0000000000001)
; kök kabul limiti
(define ok-limiti 0.000000000000001)

; turev (number -> number) -> (number -> number)
; bir tek değişken fonksiyonun sayısal approx birinci türev fonskiyonunu hesaplar
(define türev (λ (f) (λ (x) (/ (- (f (+ x delta)) (f x)) delta))))

; tamam-mı-yap? (number -> number) -> (number -> truthvalue)
; herhangi bir tek değişkenli fonksiyondan bir kök kabulu fonksiyonu yaratır 
(define tamam-mı-yap? (λ (f) (λ (t) (< (magnitude (f t)) ok-limiti))))

; iyileştir-yap (number -> number) -> (number -> number)
; Newton-Raphson yöntemi kullanarak bir tek değişkeni sayısal fonksiyonundan bir kök tahminini
; yakınlaştırmaya çalışan bir fonksiyon yaratır.
(define iyileştir-yap (λ (f f-turev) (λ (t) (- t (/ (f t) (f-turev t))))))

; bul number (number -> truthvalue) (number -> number) -> number
; bir tahmin, bir kabul fonksiyonu, bir iyileştirme fonksiyonu alarak, kabul edilir bir değer bulur 
(define bul
  (lambda (t tamam-mı? iyileştir)
    (cond
      ((tamam-mı? t) t)
      (else (bul (iyileştir t) tamam-mı? iyileştir)))))

; bul-kök (number -> number) number -> number
; bir tek değişkenli fonksiyon ve bir tahminden, bul ve newton-raphson yöntemi kullanarak
; fonksiyonun bir kökünü bulmaya çalışır.
(define bul-kök (λ (f t) (bul t (tamam-mı-yap? f) (iyileştir-yap f (türev f)))))

; karekök number number -> number
; karekökü bulunacak sayı ve bir tahminden bir karekök bulur.
(define karekök (λ (k t) (bul-kök (λ (x) (- (* x x) k)) t)))