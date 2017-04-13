#lang plai-typed

(define-type arith
  [arith-num (n : number)]
  [arith-symbol (s : symbol)]
  [arith-fonk-uygula (f-ismi : symbol) (f-param : arith)]
  [arith-add (lhs : arith) (rhs : arith)]
  [arith-mul (lhs : arith) (rhs : arith)]
  [arith-eğer-sıfır (şart : arith) (ozaman : arith) (yoksa : arith)]
  )

;; parse s-expression -> arith
;; convert a quoted Racket style arithmetic s-expression into the equivalent arith form
;; examples
;; '7 -> (arith-num 7)
;; '(+ 3 4) -> (arith-add (arith-num 3) (arith-num 4))
;; '(+ (+ 3 4) 35) -> (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35))
(define (parse [s : s-expression]) : arith
  (cond
    [(s-exp-symbol? s) (arith-symbol (s-exp->symbol s))]
    [(s-exp-number? s) (arith-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (arith-add (parse (second sl)) (parse (third sl)))]
         [(*) (arith-mul (parse (second sl)) (parse (third sl)))]
         [(eğer-sıfır) (arith-eğer-sıfır (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (arith-fonk-uygula (s-exp->symbol (first sl)) (parse (second sl)))]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (arith-num 7))
(test (parse '(+ 3 4)) (arith-add (arith-num 3) (arith-num 4)))
(test (parse '(+ (+ 3 4) 35)) (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)))

;; output-reverse-polish arith -> list of s-expression
;; output the arith as the reverse polish commands needed to evaluate it
;; examples
;; (arith-num 7) -> '(7)
;; (arith-add (arith-num 3) (arith-num 4)) -> '(4 3 +)
;; (arith-mul (arith-num 3) (arith-num 4)) -> '(4 3 *)
;; (arith-add (arith-mul (arith-num 3) (arith-num 4)) (arith-num 9)) -> '(3 4 * 9 +)
;; (arith-mul (arith-num 3) (arith-add (arith-num 4) (arith-num 9))) -> '(3 4 9 + *)
;(define (output-reverse-polish [expr : arith])
;  (type-case arith expr
;    [arith-num (n) (list (number->s-exp n))]
;    [arith-add (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
;                               (list (symbol->s-exp '+)))]
;    [arith-mul (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
;                               (list (symbol->s-exp '*)))]))
;

;(test (output-reverse-polish (arith-num 7)) (s-exp->list '(7)))
;(test (output-reverse-polish (arith-add (arith-num 3) (arith-num 4))) (s-exp->list '(3 4 +)))
;(test (output-reverse-polish (arith-mul (arith-num 3) (arith-num 4))) (s-exp->list '(3 4 *)))
;(test (output-reverse-polish (arith-add (arith-mul (arith-num 3) (arith-num 4)) (arith-num 9))) (s-exp->list '(3 4 * 9 +)))
;(test (output-reverse-polish (arith-mul (arith-num 3) (arith-add (arith-num 4) (arith-num 9)))) (s-exp->list '(3 4 9 + *)))


;"Example outputs"
;(output-reverse-polish (arith-num 7))
;(output-reverse-polish (arith-add (arith-num 3) (arith-num 4)))
;(output-reverse-polish (arith-mul (arith-num 3) (arith-num 4)))
;(output-reverse-polish (arith-add (arith-mul (arith-num 3) (arith-num 4)) (arith-num 9)))
;(output-reverse-polish (arith-mul (arith-num 3) (arith-add (arith-num 4) (arith-num 9))))
;
;
;"Parser -> reverse polish output" 
;(output-reverse-polish (parse '(+ 99 (* 5 8))))




;(define (output-bracketed [expr : arith])
;  (type-case arith expr
;    [arith-num (n) (number->s-exp n)]
;    [arith-add (lhs rhs) (list->s-exp (list (output-bracketed lhs) (symbol->s-exp '+)
;                               (output-bracketed rhs)))]
;    [arith-mul (lhs rhs) (list->s-exp (list (output-bracketed lhs) (symbol->s-exp '*)
;                               (output-bracketed rhs)))]))
;
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


;; eval arith -> number
;; evaluate an arith expression
;; examples
;; (arith-num 7) -> 7
;; (arith-add (arith-num 3) (arith-num 4)) -> 7
;; (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)) -> 42
;(define (eval [expr : arith])
;  (integer->java-int (type-case arith expr
;    [arith-num (n) n]
;    [arith-add (lhs rhs) (+ (eval lhs) (eval rhs))]
;    [arith-mul (lhs rhs) (* (eval lhs) (eval rhs))]
;    [arith-eğer-sıfır (şart ozaman yoksa)
;                      (if (= (eval şart) 0) (eval ozaman) (eval yoksa))])))
;
;(test (eval (arith-num 7))  7)
;(test (eval (arith-add (arith-num 3) (arith-num 4)))  7)
;(test (eval (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)))  42)
;
;
;"Parser -> evaluation" 
;(eval (parse '(+ 99 (* 5 8))))

(define (ikame [orijinal : arith] [ikame-edilecek : symbol] [ikame-eden : arith])
  (type-case arith orijinal
    [arith-num (n) (arith-num n)]
    [arith-symbol (s) (if (symbol=? s ikame-edilecek) ikame-eden (arith-symbol s))]
    [arith-add (lhs rhs) (arith-add (ikame lhs ikame-edilecek ikame-eden)
                            (ikame rhs ikame-edilecek ikame-eden))]
    [arith-mul (lhs rhs) (arith-mul (ikame lhs ikame-edilecek ikame-eden)
                            (ikame rhs ikame-edilecek ikame-eden))]
    [arith-eğer-sıfır (şart ozaman yoksa)
                      (arith-eğer-sıfır (ikame şart ikame-edilecek ikame-eden)
                          (ikame ozaman ikame-edilecek ikame-eden)
                          (ikame yoksa ikame-edilecek ikame-eden))]
    [arith-fonk-uygula (f-name f-param)
                       (arith-fonk-uygula f-name (ikame f-param ikame-edilecek ikame-eden))]))
