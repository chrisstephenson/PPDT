#lang racket
(require (lib "racket/draw"))
(define tolerans 0.01)
(define hayatlar 20)

(define tamam-mı-yap? (λ (f) (λ (t) (< (magnitude (f t)) tolerans))))

(define iyileştir-yap (λ (f f-turev) (λ (t) (- t (/ (f t) (f-turev t))))))

(define bul
  (lambda (tamam-mı? iyileştir hayatlar t)
    (cond
      ((<= hayatlar 0) (list hayatlar t))
      ((tamam-mı? t) (list hayatlar t))
      (else (bul tamam-mı? iyileştir (- hayatlar 1) (iyileştir t))))))

(define sıfırlar
  (λ (n)
    (cond
      ((<= n 0) empty)
      (else (cons 0 (sıfırlar (- n 1)))))))

(define pad-polinom
  (λ (p uzunluk) (append p (sıfırlar (- uzunluk (length p))))))

(define topla-polinom
  (λ (p1 p2) (map + (pad-polinom p1 (length p2)) (pad-polinom p2 (length p1)))))

(define çarp-polinom-tek-değer (λ (n p) (map (curry * n) p)))

(define çarp-polinom
  (λ (p1 p2)
    (foldr (λ (l r) (topla-polinom (çarp-polinom-tek-değer l p1) (cons 0 r))) empty p2)))

(define kökler->polinom (λ (kl) (foldr çarp-polinom '(1) (map (λ (k) (list k -1)) kl))))

(define polinom->fonksiyon (λ (p) (λ (x) (foldr (λ (l r) (+ l (* x r))) 0 p))))

(define polinom-turev (λ (p) (map * (range 1 (length p)) (rest p))))

(define (polinom-kök-bul-yap kök-listesi hayatlar)
  (let*
      ((p (kökler->polinom kök-listesi))
       (f (polinom->fonksiyon p))
       (f-turev (polinom->fonksiyon (polinom-turev p)))
       (tamam? (tamam-mı-yap? f))
       (iyileştir (iyileştir-yap f f-turev)))
    (λ (tahmin)
      (bul tamam? iyileştir hayatlar tahmin))))

(define (kök->kök-endeksi kök-listesi bulunan)
  (second
   (foldr
   (λ (l r)
     (let ((mesafe (magnitude (- bulunan l))))
       (cond
         ((< mesafe (first r)) (list mesafe (- (third r) 1) (- (third r) 1)))
         (else (list (first r) (second r) (- (third r) 1))))))
   (list +inf.f (length kök-listesi) (length kök-listesi))
   kök-listesi)))

(define (range-2d x-sayı y-sayı) (map (λ (i-y) (map (λ (i-x) (list i-x i-y)) (range x-sayı))) (range y-sayı)))
        
(define (map-2d f ll) (map (λ (l) (map f l)) ll))

(define (map-2d-2l f lla llb) (map (λ (la lb) (map f la lb)) lla llb))

(define (tahminler-yarat x-sayı y-sayı x-start y-start x-son y-son)
  (let ((x-step (/ (- x-son x-start) x-sayı))(y-step (/ (- y-son y-start) y-sayı)))
  (map-2d (λ (c) (+ x-start (* (+ (first c) 0.5) x-step) (* 0+i (+ y-start (* (+ (second c) 0.5) y-step)))))(range-2d x-sayı y-sayı))))

(define (tahmin->kök-endeksi-yap kök-listesi)
  (let ((p-bul (polinom-kök-bul-yap kök-listesi hayatlar)))
    (λ (tahmin)
      (let ((result (p-bul tahmin)))
        (cond
          ((<= (first result) 0) (length kök-listesi))
          (else (kök->kök-endeksi kök-listesi (second result))))))))

(define (kök-map kök-listesi x-sayı y-sayı x-start y-start x-son y-son)
  (map-2d (tahmin->kök-endeksi-yap kök-listesi) (tahminler-yarat x-sayı y-sayı x-start y-start x-son y-son)))

(define (yut x) (display ""))

;(kök-map (list 1+1i -1+i 1-i -1-i) 40 40 -2 -2 2 2)

(define (list-head l n)
  (cond
    ((<= n 0) empty)
    (else (cons (first l) (list-head (rest l) (sub1 n))))))

(define master-color-list (list (make-color 255 0 0) (make-color 0 255 0) (make-color 0 0 255 ) 
                                (make-color 0 255 255) (make-color 255 0 255) (make-color 255 255 0)                               
                                (make-color 0 255 128) (make-color 128 0 255) (make-color 255 128 0)
                                (make-color 0 128 255) (make-color 255 0 128) (make-color 128 255 0)
                                (make-color 0 0 0)))
(define color-list (list (make-color 255 0 0) (make-color 0 255 0) (make-color 0 0 255 ) (make-color 255 255 0) (make-color 0 0 0)))
(define (make-color-list n) (append (list-head master-color-list n) (list (make-color 0 0 0))))

(define (set-pixels dc ll-renkler ll-coords)
  (yut (map-2d-2l (λ (renk coords) (send dc set-pixel (first coords) (second coords) renk)) ll-renkler ll-coords)))

(define (do-newton kök-list x-sayı y-sayı file-name)
  (let* ((bm (make-bitmap x-sayı y-sayı))
         (dc (new bitmap-dc% [bitmap bm]))
         (aspect (/ x-sayı y-sayı))
         (y-lim 2.0)
         (x-lim (* y-lim aspect)))
    (begin (set-pixels dc
                       (map-2d (curry list-ref (make-color-list (length kök-list))) (kök-map kök-list x-sayı y-sayı (- x-lim) (- y-lim) x-lim y-lim))
                       (range-2d x-sayı y-sayı))
           (send bm save-file file-name 'png)
           bm)))

;(do-newton (list 1+1i -1+i 1-i -1-i) 400 400 "test4040.png")
(do-newton (list 1+0.9i -1+i 1-i -1-i 0.5+1.5i -0.5-1.5i) 800 600 "ld01.png")
;(do-newton (list 1+0.9i -1+i 1-i -1-i 0.5+1.5i -0.5-1.5i) 1920 1080 "hd01.png")
