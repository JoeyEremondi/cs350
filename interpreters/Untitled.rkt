#lang plait

;; Curly-Cond: A programming language with conditionals

;; BNF for Curly-Arith
;; An expression is either a number (constant/literal),
;; the sum of two other expressions, or the product of two other expressions
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | number

;; Abstract syntax for Curly-Arith
;; Represents expressions in our interpreter
(define-type Expr
  ;; Constant numbers
  (NumLit [n : Number])
  ;; {+ e1 e2}
  (Plus [left : Expr]
        [right : Expr])
  ;; {* e1 e2}
  (Times [left : Expr]
         [right : Expr]))

;; Parse
;; Takes an S-expression and turns it into an Expr
;; Raises an error if 
(define (parse [s : S-Exp]) : Expr
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (NumLit (s-exp->number s))]
    ;; {+ s1 s2}
    [(s-exp-match? `{+ ANY ANY} s)
     (Plus (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; {* s1 s2}
    [(s-exp-match? `{* ANY ANY} s)
     (Times (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

;; Evaluate Expressions
(define (interp [e : Expr] ) : Number
  (type-case Expr e
             ;; A number evaluates to itself
             [(NumLit n) n]
             ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
             [(Plus l r)
                (+ (interp l) (interp r))]
             ;; Works the same but for times
             [(Times l r)
                (* (interp l) (interp r))]))

;; The Language Pipeline
;; We run  program by parsing an s-expression
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
(define (run s-exp) (interp (parse s-exp)))

(test (run `3) 3)
(test (run `{+ 1 2}) 3)
(test (run `{* 2 {+ 3 5}}) 16)

(test/exn (parse `{1 2}) "invalid input")
