#lang flit

;; Curly-Desugar: A programming language with subtraction and equality

;; BNF for Curly-Desugar 
;; Adds subtraction, boolean operations, and equality comparison by desugaring
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "=" <expr> <expr> "}"
;;   | "{" "-" <expr> <expr> "}"
;;   | "{" "and" <expr> <expr> "}"
;;   | "{" "or" <expr> <expr> "}"
;;   | "{" "not" <expr> "}"
;;   | "{" "if" <expr> <expr> <expr> "}"
;;   | "{" "zero?" <expr> "}"
;;   | NUMBER
;;   | BOOLEAN

;; NEW
;; Intermediate Abstract Syntax
;; We desugar this into Exp
(define-type ExpS
  ;; Constant numbers
  (numS [n : Number])
  ;; Constant Booleans
  (boolS [b : Boolean])
  ;; {+ e1 e2}
  (plusS [left : ExpS]
         [right : ExpS])
  ;; {* e1 e2}
  (timesS [left : ExpS]
          [right : ExpS])
  ;; {if e1 e2 e3}
  (cndS [test : ExpS]
        [thenCase : ExpS]
        [elseCase : ExpS])
  (zero?S [e : ExpS])
  ;; NEW
  ;; This is NOT in Exp
  (subS [l : ExpS]
        [r : ExpS])
  (eqS [l : ExpS]
       [r : ExpS])
  (andS [l : ExpS]
        [r : ExpS])
  (orS [l : ExpS]
       [r : ExpS])
  (notS [e : ExpS]))

;; Abstract syntax for Curly-Cond
;; Represents expressions in our interpreter
(define-type Exp
  ;; Constant numbers
  (numE [n : Number])
  ;; Constant Booleans
  (boolE [b : Boolean])
  ;; {+ e1 e2}
  (plusE [left : Exp]
         [right : Exp])
  ;; {* e1 e2}
  (timesE [left : Exp]
          [right : Exp])
  ;; {if e1 e2 e3}
  (cndE [test : Exp]
        [thenCase : Exp]
        [elseCase : Exp])
  (zero?E [e : Exp]))

;; We now allow values to be either Numbers or Booleans,
;; So we define a datatype for possible values
(define-type Value
  [numV (n : Number)]
  [boolV (b : Boolean)])

;; Parse
;; Takes an S-expression and turns it into an Exp
;; Raises an error if it doesn't represent a valid program
(define (parse [s : S-Exp]) : ExpS
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (numS (s-exp->number s))]
    ;; Constant boolean e.g. #t, #f
    [(s-exp-match? `#t s) (boolS #t)]
    [(s-exp-match? `#f s) (boolS #f)]
    ;; {+ s1 s2}
    [(s-exp-match? `{+ ANY ANY} s)
     (plusS (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; {* s1 s2}
    [(s-exp-match? `{* ANY ANY} s)
     (timesS (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    ;; NEW
    ;; We can parse and desugar subtraction without changing the interpreter
    [(s-exp-match? `{- ANY ANY} s)
     (subS (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (eqS (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{and ANY ANY} s)
     (andS (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{or ANY ANY} s)
     (orS (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{not ANY} s)
     (notS (parse (second (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (cndS (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{zero? ANY} s)
     (zero?S (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Source to Source transformations on Exp,
;; implementing various operations without interpretation.
;; We use these in our desugar function below

;; Produce an expression that evaluates
;; to the difference of the values of l and r
(define (subE [l : Exp]
              [r : Exp])
  : Exp
  (plusE l
         (timesE (numE -1) r)))

;; Produce an expression that evaluates to true
;; if and only if l and r evaluates to equal numbers
(define (eq?E [l : Exp]
              [r : Exp])
  : Exp
  (zero?E (subE l r)))

;; Produce an expression that evaluates to true if and only if
;; both its input evaluate to the boolean true.
(define (andE [l : Exp]
              [r : Exp])
  : Exp
  ;; If the first one is true, then the AND is true if and only if
  ;; the second one is also true, i.e. the value of the second one.
  ;; Otherwise, if the first one is false, the AND is false
  (cndE l
        r
        (boolE #f)))

;; Produce an expression that evaluates to true
;; if either of the given expressions evaluates to true.
(define (orE [l : Exp]
             [r : Exp])
  : Exp
  ;; If the first one is true, then the OR is true.
  ;; Otherwise, the OR is true iff the second one is, i.e. it has its value.
  (cndE l
        (boolE #t)
        r))

;; Produce an expression that evaluates to the opposite boolean
;; of the given expression.
(define (notE [e : Exp])
  : Exp
  (cndE e
        (boolE #f)
        (boolE #t)))



;; NEW
;; Desugar Expressions into Core Syntax
(define (desugar [es : ExpS]) : Exp
  (type-case ExpS es
    ;; For expressions in the core syntax,
    ;; We just pattern match, desugar the parts,
    ;; and put them back together with the Exp constructor
    [(numS n)
     (numE n)]
    [(boolS b)
     (boolE b)]
    [(plusS l r)
     (plusE (desugar l) (desugar r))]
    [(timesS l r)
     (timesE (desugar l) (desugar r))]
    [(cndS test thn els)
     (cndE (desugar test)
           (desugar thn)
           (desugar els))]
    [(zero?S e)
     (zero?E (desugar e))]
    ;; The remaining features aren't covered by the core syntax
    ;; So we have to transform them into core syntax that does something equivalent.
    ;; We use the helper functions above to do so.
    [(subS l r)
     (subE (desugar l) (desugar r))]
    [(eqS l r)
     (eq?E (desugar l) (desugar r))]
    [(andS l r)
     (andE (desugar l) (desugar r))]
    [(orS l r)
     (orE (desugar l) (desugar r))]
    [(notS e)
     (notE (desugar e))]
    ))


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
     (lift-binop + (interp l) (interp r))]
    ;; Works the same but for times
    [(timesE l r)
     (lift-binop * (interp l) (interp r))]
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
(define (run s-exp) (interp (desugar
                             (parse s-exp))))

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

;; Tests for desugaring
(test (run `{- 5 3})
      (numV 2))

(test (run `{= 5 {+ 2 3}})
      (boolV #t))
(test (run `{= 6 {+ 2 3}})
      (boolV #f))

(test (run `{and {= 3 3} {= 4 5}})
      (boolV #f))
(test (run `{and {= 3 3} {= 4 4}})
      (boolV #t))

(test (run `{or {= 3 3} {= 4 5}})
      (boolV #t))
(test (run `{or {= 3 4} {= 4 5}})
      (boolV #f))

(test (run `{not {= 3 5}})
      (boolV #t))

(test (run `{if {not
                 {and
                  {= 3 5}
                  {or
                   #t
                   {= 5 22}}}}
                99
                100})
      (numV 99))
