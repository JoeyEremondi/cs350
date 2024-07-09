#lang plait

(define (myFun x)
  (+ x 3))

;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if0" <expr> <expr> <expr> "}"
;;   | number


(define-type Expr
  (NumLit [n : Number])
  (Plus [left : Expr]
        [right : Expr])
  (Times [left : Expr]
         [right : Expr])
  (If0 [test : Expr]
       [thenCase : Expr]
       [elseCase : Expr]))

(define (parse [s : S-Exp]) : Expr
  (cond
    [(s-exp-match? `NUMBER s) (NumLit (s-exp->number s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (Plus (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (Times (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (If0 (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (interp [e : Expr] ) : Number
  (type-case Expr e
             [(NumLit n) n]
             [(Plus l r)
                (+ (interp l) (interp r))]
             [(Times l r)
                (* (interp l) (interp r))]
    [(If0 test thenCase elseCase)
     (let ([testVal (interp test)])
       (if (= 0 testVal)
           (interp thenCase)
           (interp elseCase)))]))


(define (eval s-exp) (interp (parse s-exp)))
