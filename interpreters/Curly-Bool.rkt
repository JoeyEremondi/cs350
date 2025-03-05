#lang flit

;; Curly-Bool: A programming language with booleans

;; BNF for Curly-Bool
;; We have proper if instead of if0,
;; and we have #t and #f as boolean literals,
;; and an expression to check if a number is zero
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if" <expr> <expr> <expr> "}"
;;   | "{" "zero?" <expr> "}"
;;   | NUMBER
;;   | BOOLEAN

;; Abstract syntax for Curly-Cond
;; Represents expressions in our interpreter
;; NEW: we add E to the constructors
;; to show that they're Expression constructors,
;; not Value constructors
(define-type Exp
  ;; Constant numbers
  (numE [n : Number])
  ;; NEW
  ;; Constant Booleans
  (boolE [b : Boolean])
  ;; {+ e1 e2}
  (plusE [left : Exp]
         [right : Exp])
  ;; {* e1 e2}
  (timesE [left : Exp]
          [right : Exp])
  ;; NEW
  ;; {if e1 e2 e3}
  (cndE [test : Exp]
        [thenCase : Exp]
        [elseCase : Exp])
  ;; NEW
  (zero?E [e : Exp]))

;; NEW:
;; We now allow values to be either Numbers or Booleans,
;; So we define a datatype for possible values
(define-type Value
  [numV (n : Number)]
  [boolV (b : Boolean)])

;; Parse
;; Takes an S-expression and turns it into an Exp
;; Raises an error if it doesn't represent a valid program
(define (parse [s : S-Exp]) : Exp
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    ;; NEW
    ;; Constant boolean e.g. #t, #f
    [(s-exp-match? `#t s) (boolE #t)]
    [(s-exp-match? `#f s) (boolE #f)]
    ;; {+ s1 s2}
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; {* s1 s2}
    [(s-exp-match? `{* ANY ANY} s)
     (timesE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    ;; NEW: just like if0
    [(s-exp-match? `{if ANY ANY ANY} s)
     (cndE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    ;; NEW for booleans
    [(s-exp-match? `{zero? ANY} s)
     (zero?E (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

;; NEW
;; Lifting operations on Numbers to Values
(define (lift-binop [op : (Number Number -> Number)]
                    [v1 : Value]
                    [v2 : Value])
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2)
        (numV (op n1 n2))]
       [else (error 'lift-binop "expects RHS to be a number")])]
    [else (error 'lift-binop "expects LHS to be a number")]))

;; Evaluate Expressions
(define (interp [e : Exp] ) : Value
  (type-case Exp e
    ;; A number evaluates to itself
    ;; Need to wrap in numV, since result type is value
    [(numE n)
     (numV n)]
    ;; A boolean evaluates to itself
    [(boolE b)
     (boolV b)]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    [(plusE l r)
     ;; DON'T WANT this:
     ;; (type-case l
     ;;    [(numE x) ...]
     (lift-binop + (interp l) (interp r))]
    ;; Works the same but for times
    [(timesE l r)
     (lift-binop * (interp l) (interp r))]
    ;; NEW
    ;; {if test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(cndE test thn els)
     (type-case Value (interp test)
       [(boolV b)
        (if b
            (interp thn)
            (interp els))]
       [else (error 'interp "Non-boolean given to if")])]
    ;; To check if value is zero, we interpret it,
    ;; then pattern match on the result
    [(zero?E e)
     (type-case Value (interp e)
       [(numV n)
        (boolV (= n 0))]
       [else
        (error 'interp "Expected number")])]))

;; The Language Pipeline
;; We run  program by parsing an s-expression into an expression
;; then interpreting it into a number
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
(define (run s-exp) (interp (parse s-exp)))

(test (run `3)
      (numV 3))
(test (run `{+ 1 2})
      (numV 3))
(test (run `{* 2 {+ 3 5}})
      (numV 16))

(test (run `{if {zero? {+ 1 -1}} 1 2})
      (numV 1))

(test (run `{if {zero? {+ 90 9}}
                1
                2})
      (numV 2))
(test (run `{if {zero? {* 2 0}} {+ 3 5} {* 3 5}})
      (numV 8))

(test (run `{+ 3 {if #t 10 20}})
      (numV 13))

(test/exn (parse `{1 2}) "invalid input")

;; Make sure we catch dynamic type errors

(test/exn (run `{if 3 4 5}) "boolean")
(test/exn (run `{+ #t 5}) "number")
(test/exn (run `{if {if #t 4 #f} 4 5}) "boolean")
