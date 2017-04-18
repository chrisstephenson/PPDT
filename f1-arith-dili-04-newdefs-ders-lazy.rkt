#lang plai-typed

(define-type f1-program
  [f1-program-def (fonklar : (listof fonk-def)) (ar-expr : arith)]
  )

(define-type fonk-def
  [fonk-def-1 (f-ismi : symbol) (f-formal-param : symbol) (f-vucut : arith)]
  )


(define-type arith
  [arith-num (n : number)]
  [arith-symbol (s : symbol)]
  [arith-fonk-uygula (f-ismi : symbol) (f-gerçek-param : arith)]
  [arith-add (lhs : arith) (rhs : arith)]
  [arith-mul (lhs : arith) (rhs : arith)]
  [arith-eğer-sıfır-ya-eksiyse (şart : arith) (ozaman : arith) (yoksa : arith)]
  )

;; parse-arith s-expression -> arith
;; convert a quoted Racket style arithmetic s-expression into the equivalent arith form
;; examples
;; '7 -> (arith-num 7)
;; '(+ 3 4) -> (arith-add (arith-num 3) (arith-num 4))
;; '(+ (+ 3 4) 35) -> (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35))
(define (parse-arith [s : s-expression]) : arith
  (cond
    [(s-exp-symbol? s) (arith-symbol (s-exp->symbol s))]
    [(s-exp-number? s) (arith-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (arith-add (parse-arith (second sl)) (parse-arith (third sl)))]
         [(*) (arith-mul (parse-arith (second sl)) (parse-arith (third sl)))]
         [(eğer-sıfır-ya-eksiyse) (arith-eğer-sıfır-ya-eksiyse (parse-arith (second sl)) (parse-arith (third sl)) (parse-arith (fourth sl)))]
         [else (arith-fonk-uygula (s-exp->symbol (first sl)) (parse-arith (second sl)))]))]
    [else (error 'parse-arith "invalid input")]))

(test (parse-arith '7) (arith-num 7))
(test (parse-arith '(+ 3 4)) (arith-add (arith-num 3) (arith-num 4)))
(test (parse-arith '(+ (+ 3 4) 35)) (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)))

(define (parse-fonk-def [s : s-expression]) : fonk-def
  (cond
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(fonk) (fonk-def-1 (s-exp->symbol (second sl)) (s-exp->symbol(first (s-exp->list (third sl)))) (parse-arith (fourth sl)))]
         [else (error 'parse-fonk-def "invalid input")]))]
    [else (error 'parse-fonk-def "invalid input")]))

(define (parse-program [s : s-expression]) : f1-program
  (let* ((prog-input-as-l (s-exp->list s))
        (fonks-input-as-l (s-exp->list (first prog-input-as-l)))
        (ar-input (second prog-input-as-l)))
    (f1-program-def (map parse-fonk-def fonks-input-as-l) (parse-arith ar-input))))

 
(define 2^32 4294967296)
(define 2^31 2147483648)
;;
;; Emulate java 32 bit arithmetic
;;
(define (integer->java-int n)
  (let ((nmod-32 (modulo n 2^32))) 
  (cond
    ((<  nmod-32 2^31) nmod-32)
    (else (- nmod-32 2^32)))))

(define (eval-program [p : f1-program]) : number
  (type-case f1-program p
    [f1-program-def (fonklar ar-exp) (eval fonklar ar-exp)]))

(define (f-match [f : fonk-def] [isim : symbol]) : boolean
  (type-case fonk-def f
    [fonk-def-1 (n p v) (symbol=? isim n)]))

(define (fonk-bul [fl : (listof fonk-def)] [isim : symbol]) : fonk-def
  (first (filter (λ (f) (f-match f isim)) fl)))

;; lazy parameter substitution
(define (fonk-uygula-eval  [fl : (listof fonk-def)] [f : fonk-def] [gerçek-param : arith]) : number
  (type-case fonk-def f
    [fonk-def-1 (isim formal-param vucut) (eval fl (ikame vucut formal-param gerçek-param))]))



;; eval arith -> number
;; evaluate an arith expression
(define (eval [fl : (listof fonk-def)][expr : arith]) : number
  (type-case arith expr
    [arith-num (n) n]
    [arith-symbol (s) (error 'eval "unbound symbol")]
    [arith-fonk-uygula (f-ismi f-gerçek-param) (fonk-uygula-eval fl (fonk-bul fl f-ismi) f-gerçek-param)]
    [arith-add (lhs rhs) (+ (eval fl lhs) (eval fl rhs))]
    [arith-mul (lhs rhs) (* (eval fl lhs) (eval fl rhs))]
    [arith-eğer-sıfır-ya-eksiyse (şart ozaman yoksa)
                      (if (<= (eval fl şart) 0) (eval fl ozaman) (eval fl yoksa))]))

;(test (eval (arith-num 7))  7)
;(test (eval (arith-add (arith-num 3) (arith-num 4)))  7)
;(test (eval (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)))  42)

(define (ikame [orijinal : arith] [ikame-edilecek : symbol] [ikame-eden : arith])
  (type-case arith orijinal
    [arith-num (n) (arith-num n)]
    [arith-symbol (s) (if (symbol=? s ikame-edilecek) ikame-eden (arith-symbol s))]
    [arith-add (lhs rhs) (arith-add (ikame lhs ikame-edilecek ikame-eden)
                            (ikame rhs ikame-edilecek ikame-eden))]
    [arith-mul (lhs rhs) (arith-mul (ikame lhs ikame-edilecek ikame-eden)
                            (ikame rhs ikame-edilecek ikame-eden))]
    [arith-eğer-sıfır-ya-eksiyse (şart ozaman yoksa)
                      (arith-eğer-sıfır-ya-eksiyse (ikame şart ikame-edilecek ikame-eden)
                          (ikame ozaman ikame-edilecek ikame-eden)
                          (ikame yoksa ikame-edilecek ikame-eden))]
    [arith-fonk-uygula (f-name f-param)
                       (arith-fonk-uygula f-name (ikame f-param ikame-edilecek ikame-eden))]))


;;tests
(test (eval-program (parse-program '(((fonk fact (n) (eğer-sıfır-ya-eksiyse n 1 (* n (fact (+ n -1))))))
                                     (fact 7)))) 5040)
(test (eval-program (parse-program '(((fonk fib (n) (eğer-sıfır-ya-eksiyse (+ n -1) 1 (+(fib (+ n -1))(fib (+ n -2))))))
                                     (fib 9)))) 55)
(test (eval-program (parse-program '((
                                      (fonk fib (n) (eğer-sıfır-ya-eksiyse (+ n -1) 1 (+(fib (+ n -1))(fib (+ n -2)))))
                                      (fonk fact (n) (eğer-sıfır-ya-eksiyse n 1 (* n (fact (+ n -1))))))
                                     (fib 9)))) 55)

