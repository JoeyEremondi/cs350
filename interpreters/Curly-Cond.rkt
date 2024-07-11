#lang plait

;; Curly-Cond: A programming language with conditionals

;; BNF for Curly-Cond
;; New form: {if0 test thn els}
;; Evaluates to thn whenever test has value 0
;; otherwise evaluates to els
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if0" <expr> <expr> <expr> "}"
;;   | NUMBER

;; Abstract syntax for Curly-Cond
;; Represents expressions in our interpreter
(define-type Expr
  ;; Constant numbers
  (NumLit [n : Number])
  ;; {+ e1 e2}
  (Plus [left : Expr]
        [right : Expr])
  ;; {* e1 e2}
  (Times [left : Expr]
         [right : Expr])
  ;; NEW
  ;; {if0 e1 e2 e3}
  (If0 [test : Expr]
       [thenCase : Expr]
       [elseCase : Expr]))

;; Parse
;; Takes an S-expression and turns it into an Expr
;; Raises an error if it doesn't represent a valid program
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
    ;; NEW
    ;; Same idea as above, but we're looking for 3 arguments, not 2, for if0
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (If0 (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
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
     (* (interp l) (interp r))]
    ;; NEW
    ;; {if0 test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(If0 test thn els)
       (if (= 0 (interp test))
           (interp thn)
           (interp els))]))

;; The Language Pipeline
;; We run  program by parsing an s-expression into an expression
;; then interpreting it into a number
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
(define (run s-exp) (interp (parse s-exp)))

(test (run `3)
      3)
(test (run `{+ 1 2})
      3)
(test (run `{* 2 {+ 3 5}})
      16)

(test (run `{if0 0 1 2})
      1)
(test (run `{if0 99 1 2})
      2)
(test (run `{if0 {* 2 0} {+ 3 5} {* 3 5}})
      8)

(test (run `{+ 3 {if0 3 10 20}})
      23)

(test/exn (parse `{1 2}) "invalid input")
