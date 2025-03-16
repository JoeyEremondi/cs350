#lang flit

;; Curly-Let: A programming language with subtraction and equality

;; BNF for Curly-Let
;; Adds subtraction, boolean operations, and equality comparison by desugaring
;; 
;;  <expr> ::=
;;   | NUMBER
;;   | BOOLEAN
;;   | VARIABLE ;; NEW
;;   | { + <expr> <expr> }
;;   | { * <expr> <expr> }
;;   | { = <expr> <expr> }
;;   | { - <expr> <expr> }
;;   | { and <expr> <expr> }
;;   | { or <expr> <expr> }
;;   | { not <expr> }
;;   | { if <expr> <expr> <expr> }
;;   | { zero? <expr> }
;;   | {let1 {VARIABLE <expr>} <expr> }
;;   ;; NEW
;;   | {lam VARIABLE <expr>} ;; function definition
;;   | {<expr> <expr>} ;; function calling

;; The expression {let1 {x e1} e2} means "x has value e1 in e2"


;; Intermediate Abstract Syntax
;; We desugar this into Exp
(define-type ExpS
  ;; Constant numbers
  (numS [n : Number])
  ;; Constant Booleans
  (boolS [b : Boolean])
  ;; Variables ;; NEW
  ;; Represent variables as Flit symbols
  ;;  that we can compare with symbol=?
  (varS [x : Symbol])
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
  (subS [l : ExpS]
        [r : ExpS])
  (eqS [l : ExpS]
       [r : ExpS])
  (andS [l : ExpS]
        [r : ExpS])
  (orS [l : ExpS]
       [r : ExpS])
  (notS [e : ExpS])
  ;; Let-expressions
  (let1S [var : Symbol]
         [val : ExpS]
         [body : ExpS])
  ;; Functions and function calls (application)
  (lamS [var : Symbol]
        [body : ExpS])
  (appS [fun : ExpS] [arg : ExpS])
  )

;; Abstract syntax for Curly-Cond
;; Represents expressions in our interpreter
(define-type Exp
  ;; Constant numbers
  (numE [n : Number])
  ;; Constant Booleans
  (boolE [b : Boolean])
  ;; NEW
  ;; Variables are in the core language
  (varE [x : Symbol])
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
  (zero?E [e : Exp])
  ;; Let-expressions are in the core language
  (let1E [var : Symbol]
         [val : Exp]
         [body : Exp])
  ;; Functions and applications (function calls) are in the core language
  (lamE [var : Symbol] [vody : Exp])
  (appE [fun : Exp] [arg : Exp])
  )

;; We now allow values to be either Numbers, Booleans, or Functions
(define-type Value
  [numV (n : Number)]
  [boolV (b : Boolean)]
  ;; NEW
  ;; Note that functions as values contain the exact same data as function expressions.
  ;; Now Values and Expressions are mutually defined.
  [lamV (var : Symbol)
        (body : Exp)])

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
    ;; NEW
    ;; Variables e.g. x
    [(s-exp-match? `SYMBOL s) (varS (s-exp->symbol s))]
    ;; {+ s1 s2}
    [(s-exp-match? `{+ ANY ANY} s)
     (plusS (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; {* s1 s2}
    [(s-exp-match? `{* ANY ANY} s)
     (timesS (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
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
    ;; Variable definitions
    ;; {let1 {x val} body}
    [(s-exp-match? `{let1 {SYMBOL ANY} ANY} s)
     (let1S (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
            (parse (second (s-exp->list (second (s-exp->list s)))))
            (parse (third (s-exp->list s))))]
    ;; NEW:
    ;; Lambdas and function calls
    [(s-exp-match? `{lam SYMBOL ANY} s)
     (lamS (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; Catch-all case for function calls/applications
    [(s-exp-match? `{ANY ANY} s)
     (appS (parse (first (s-exp->list (second (s-exp->list s)))))
           (parse (second (s-exp->list (second (s-exp->list s))))))]
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
    ;; No interesting desugaring to do for Variables and Let,
    ;; but note we don't need to recursively call (desugar ...)
    ;; on the symbol itself
    [(varS x)
     (varE x)]
    [(let1S var val body)
     (let1E var (desugar val) (desugar body))]
    ;; No interesting desugaring for functions, just recursively desugar the parts
    [(lamS x body)
     (lamE x (desugar body))]
    [(appS fun args)
     (appE (desugar fun) (desugar args))]
    ))

;; NEW
;;;;;;;;;;;;;;;;;;;;;;
;; Substitution
;; Replace the variable with the given expression
;; in some other expression e.
;; Has shadowing: we don't replace a variable x
;; in a scope where x is bound (redefined).

(define (subst [toReplace : Symbol]
               [replacedBy : Exp]
               [e : Exp])
  : Exp
  (type-case Exp e
    ;; Variable case
    ;; If the variable is the one we're trying to replace,
    ;; then replace it, otherwise just leave it unchanged.
    [(varE x)
     (if (symbol=? x toReplace)
         replacedBy ;; variable match, so replace with new expression
         (varE x))] ;; didn't match, so just leave the variable unchanged
    ;; Literals don't contain variables,
    ;; so no replacing to be done
    [(numE n)
     (numE n)]
    [(boolE b)
     (boolE b)]
    ;; Remaining expressions: they don't (directly) bind or use variables,
    ;; so we just recursively substitute in the parts
    [(plusE l r)
     (plusE (subst toReplace replacedBy l)
            (subst toReplace replacedBy r))]
    [(timesE l r)
     (timesE (subst toReplace replacedBy l)
             (subst toReplace replacedBy r))]
    [(cndE test thn els)
     (cndE (subst toReplace replacedBy test)
           (subst toReplace replacedBy thn)
           (subst toReplace replacedBy els))]
    [(zero?E e)
     (zero?E (subst toReplace replacedBy e))]
    ;; Function calls work just like normal
    [(appE fun arg)
       (appE (subst toReplace replacedBy fun)
             (subst toReplace replacedBy arg))]
    ;; Shadowing:
    ;; {let1 {x val} body} *binds* x in body,
    ;; so if we're replacing x in the let1 expression,
    ;; we dont replace it in the body, since we're defining a "new"
    ;; x that shadows the old one
    [(let1E var val body)
     (let1E var
            (subst toReplace replacedBy val)
            (if (symbol=? var toReplace)
                body
                (subst toReplace replacedBy body)))]
  ;; NEW:
  ;; Shadowing on functions works like on let:
  ;; don't replace in the body if the function's variable is what we're replacing,
  ;; so that the function variable shadows the outer variable
  [(lamE var body)
   (lamE var
         (if (symbol=? var toReplace)
             body
             (subst toReplace replacedBy body)))]
  ))


;; Substitution works with expressions,
;; so we need a way to convert values back to expressions.
;; We just convert boolean values to literals, same with numbers.
(define (Value->Exp [v : Value])
  : Exp
  (type-case Value v
    [(boolV b)
     (boolE b)]
    [(numV n)
     (numE n)]
    ;; NEW
    ;; We can turn function values into function expressions,
    ;; since they contain the exact same data, we just change the constructor
    [(lamV x body)
       (lamE x body)]))

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
        (error 'interp "Expected number")])]
    ;; To interpret a let-expression,
    ;; we evaluate the value for the variable.
    ;; Then we replace the variable with that expression everywhere
    ;; in the body, and evaluate the body.
    [(let1E var val body)
     (let* ([valV (interp val)]
            [subbedBody (subst var (Value->Exp valV) body)])
       (interp subbedBody))]
    ;; If we encounter a variable that we haven't substituted away,
    ;; then we must have had an undefined variable error
    [(varE x)
     (error 'interp (string-append "Undefined variable: " (symbol->string x)))]
    ;; NEW
    ;; Interpreting functions is a no-op: we just wrap the function as a value.
    ;; This is because functions store computation to be done later, after some substitutions.
    [(lamE var body)
       (lamV var body)]
    ;; NEW
    ;; Interpreting function calls (applications)
    ;; Two things to do: make sure the thing we're calling is a function,
    ;; then replace its variable with the argument's value in the body.
    ;; Finally, we interpret the body.
    [(appE fun arg)
       (type-case Value (interp fun)
         [(lamV var body)
            ;; Do the substitution,
            ;; then interpret the result
            (let* ([argExp (Value->Exp (interp arg))]
                   [subbedBody (subst var argExp body)])
              (interp subbedBody))]
         [else
            (error 'interp "Tried to call non-function")])]))

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

;; NEW
(test (run
       `{let1 {x {+ 9900 99}}
              {+ x {* x {if {zero? x} x {- x x}}}}})
      (numV 9999))
