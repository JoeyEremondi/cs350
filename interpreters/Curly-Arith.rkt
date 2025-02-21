#lang flit

;; Curly-Arith: Creating our first programming language
;; A simple language with numbers, addition and multiplication

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
(define-type Exp
  ;; Constant numbers
  (num [n : Number])
  ;; {+ e1 e2}
  (plus [left : Exp]
        [right : Exp])
  ;; {* e1 e2}
  (times [left : Exp]
         [right : Exp]))

;; Value is the type that our interpreter produces
;; We don't need to define a datatype, we can just use Number.
;; We define a type alias so that we can swap Number out for
;; more interesting types later on.
(define-type-alias Value Number)

;; Parse
;; Takes an S-expression and turns it into an Expr
;; Raises an error if it's not a valid Curly-Arith program
;;
;; parse : S-Exp -> Exp
(define (parse [s : S-Exp])
  : Exp
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (num (s-exp->number s))]
    ;; {+ s1 s2}
    [(s-exp-match? `{+ ANY ANY} s)
     (plus (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    ;; {* s1 s2}
    [(s-exp-match? `{* ANY ANY} s)
     (times (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))



;; Evaluate Expressions
;; The texbook calls this cal for arithmetic
;;
;; interp : Exp -> Value
(define (interp [e : Exp] ) : Value
  (type-case Exp e
    ;; A number evaluates to itself
    [(num n) n]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    [(plus l r)
     (+ (interp l) (interp r))]
    ;; Works the same but for times
    [(times l r)
     (* (interp l) (interp r))]))

;; The Language Pipeline
;; We run  program by parsing an s-expression
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
;;
;; run : S-Exp -> Number
(define (run s-exp) (interp (parse s-exp)))

(test (run `3) 3)
(test (run `{+ 1 2}) 3)
(test (run `{* 2 {+ 3 5}}) 16)

(test/exn (parse `{1 2}) "invalid input")
