#lang plai-typed

(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  )

;; eval msl -> number
;; evaluate an msl expression
;; examples
;; (msl-num 7) -> 7
;; (msl-add (msl-num 3) (msl-num 4)) -> 7
;; (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)) -> 42
(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]))

(test (eval (msl-num 7))  7)
(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)


;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))



;; output-reverse-polish msl -> list of s-expression
;; output the msl as the reverse polish commands needed to evaluate it
;; examples
;; (msl-num 7) -> '(7)
;; (msl-add (msl-num 3) (msl-num 4)) -> '(4 3 +)
;; (msl-mul (msl-num 3) (msl-num 4)) -> '(4 3 *)
;; (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)) -> '(3 4 * 9 +)
;; (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9))) -> '(3 4 9 + *)
(define (output-reverse-polish [expr : msl])
  (type-case msl expr
    [msl-num (n) (list (number->s-exp n))]
    [msl-add (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
                               (list (symbol->s-exp '+)))]
    [msl-mul (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
                               (list (symbol->s-exp '*)))]))


(test (output-reverse-polish (msl-num 7)) (s-exp->list '(7)))
(test (output-reverse-polish (msl-add (msl-num 3) (msl-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish (msl-mul (msl-num 3) (msl-num 4))) (s-exp->list '(3 4 *)))
(test (output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9))) (s-exp->list '(3 4 * 9 +)))
(test (output-reverse-polish (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9)))) (s-exp->list '(3 4 9 + *)))


"Example outputs"
(output-reverse-polish (msl-num 7))
(output-reverse-polish (msl-add (msl-num 3) (msl-num 4)))
(output-reverse-polish (msl-mul (msl-num 3) (msl-num 4)))
(output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)))
(output-reverse-polish (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9))))


"Parser -> reverse polish output" 
(output-reverse-polish (parse '(+ 99 (* 5 8))))


"Parser -> evaluation" 
(eval (parse '(+ 99 (* 5 8))))


(define (output-bracketed [expr : msl])
  (type-case msl expr
    [msl-num (n) (number->s-exp n)]
    [msl-add (lhs rhs) (list->s-exp (list (output-bracketed lhs) (symbol->s-exp '+)
                               (output-bracketed rhs)))]
    [msl-mul (lhs rhs) (list->s-exp (list (output-bracketed lhs) (symbol->s-exp '*)
                               (output-bracketed rhs)))]))


