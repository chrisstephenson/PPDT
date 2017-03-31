#lang plai-typed

(define-type arith
  [arith-num (n : number)]
  [arith-add (lhs : arith) (rhs : arith)]
  [arith-mul (lhs : arith) (rhs : arith)]
  )

;; parse s-expression -> arith
;; convert a quoted Racket style arithmetic s-expression into the equivalent arith form
;; examples
;; '7 -> (arith-num 7)
;; '(+ 3 4) -> (arith-add (arith-num 3) (arith-num 4))
;; '(+ (+ 3 4) 35) -> (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35))
(define (parse [s : s-expression]) : arith
  (cond
    [(s-exp-number? s) (arith-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (arith-add (parse (second sl)) (parse (third sl)))]
         [(*) (arith-mul (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
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
(define (output-reverse-polish [expr : arith])
  (type-case arith expr
    [arith-num (n) (list (number->s-exp n))]
    [arith-add (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
                               (list (symbol->s-exp '+)))]
    [arith-mul (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
                               (list (symbol->s-exp '*)))]))


(test (output-reverse-polish (arith-num 7)) (s-exp->list '(7)))
(test (output-reverse-polish (arith-add (arith-num 3) (arith-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish (arith-mul (arith-num 3) (arith-num 4))) (s-exp->list '(3 4 *)))
(test (output-reverse-polish (arith-add (arith-mul (arith-num 3) (arith-num 4)) (arith-num 9))) (s-exp->list '(3 4 * 9 +)))
(test (output-reverse-polish (arith-mul (arith-num 3) (arith-add (arith-num 4) (arith-num 9)))) (s-exp->list '(3 4 9 + *)))


"Example outputs"
(output-reverse-polish (arith-num 7))
(output-reverse-polish (arith-add (arith-num 3) (arith-num 4)))
(output-reverse-polish (arith-mul (arith-num 3) (arith-num 4)))
(output-reverse-polish (arith-add (arith-mul (arith-num 3) (arith-num 4)) (arith-num 9)))
(output-reverse-polish (arith-mul (arith-num 3) (arith-add (arith-num 4) (arith-num 9))))


"Parser -> reverse polish output" 
(output-reverse-polish (parse '(+ 99 (* 5 8))))




(define (output-bracketed [expr : arith])
  (type-case arith expr
    [arith-num (n) (number->s-exp n)]
    [arith-add (lhs rhs) (list->s-exp (list (output-bracketed lhs) (symbol->s-exp '+)
                               (output-bracketed rhs)))]
    [arith-mul (lhs rhs) (list->s-exp (list (output-bracketed lhs) (symbol->s-exp '*)
                               (output-bracketed rhs)))]))


;; eval arith -> number
;; evaluate an arith expression
;; examples
;; (arith-num 7) -> 7
;; (arith-add (arith-num 3) (arith-num 4)) -> 7
;; (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)) -> 42
(define (eval [expr : arith])
  (type-case arith expr
    [arith-num (n) n]
    [arith-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [arith-mul (lhs rhs) (* (eval lhs) (eval rhs))]))

(test (eval (arith-num 7))  7)
(test (eval (arith-add (arith-num 3) (arith-num 4)))  7)
(test (eval (arith-add (arith-add (arith-num 3) (arith-num 4)) (arith-num 35)))  42)


"Parser -> evaluation" 
(eval (parse '(+ 99 (* 5 8))))
