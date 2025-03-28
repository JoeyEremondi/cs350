#lang flit

;; Curly-Env
;; This language is identical to Curly-Curry,
;; but implemented using environments, rather than substitution.
;; This make it more efficient, and has the advantage that
;; we can say that programs with variables have context-dependent meaning,
;; rather than just being errors.

;; BNF 
;; 
;;  <expr> ::=
;;   | NUMBER
;;   | BOOLEAN
;;   | VARIABLE 
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
;;   | {lam VARIABLE <expr>} ;; function definition
;;   | {lam {VARIABLE*} <expr>} ;; function definition
;;   | {<expr> <expr>*} ;; function calling




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW
;; Code for environments
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments are association lists,
;; e.g. symbol-value pairs, that we can
;; make empty lists, insert into, and lookup the value
;; for a certain symbol

;; Association lists are lists of key-vaue pairs
(define-type-alias (AssocList 'key 'value)
  (Listof ('key * 'value)))

;; We insert by adding to the beginning of a list
(define (extend [k : 'key]
                [v : 'value]
                [al : (AssocList 'key 'value)])
  : (AssocList 'key 'value)
  (cons (pair k v) al))

;; To find an entry in the list, we take the first (most recently added)
;; entry whose key matche the given key
(define (lookup [k : 'key]
              [al : (AssocList 'key 'value)])
  : 'value
  ;; Filter out all the pairs whose first element is the same as the given key
  (let* ([filteredList (filter
                        (lambda (pr)
                          (equal? (pairFst pr) k))
                        al)])
    ;; Take the first one, or raise an error if none present
    (if (empty? filteredList)
        (error 'find "Didn't find key in list")
        (pairSnd (first filteredList)))))




;; Intermediate Abstract Syntax
;; We desugar this into Exp
(define-type ExpS
  ;; Constant numbers
  (numS [n : Number])
  ;; Constant Booleans
  (boolS [b : Boolean])
  ;; Variables 
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
  ;; NEW:
  ;; Surface language has n-ary function calls and lambdas
  ;; These get desugared away
  (lamS [var : (Listof Symbol)]
        [body : ExpS])
  (appS [fun : ExpS] [args : (Listof ExpS)])
  )

;; Abstract syntax for Curly-Cond
;; Represents expressions in our interpreter
(define-type Exp
  ;; Constant numbers
  (numE [n : Number])
  ;; Constant Booleans
  (boolE [b : Boolean])
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
  ;; Functions and applications (function calls) are in the core language
  (lamE [var : Symbol] [vody : Exp])
  (appE [fun : Exp] [arg : Exp])
  )

;; We now allow values to be either Numbers, Booleans, or Functions
(define-type Value
  [numV (n : Number)]
  [boolV (b : Boolean)]
  ;; NEW
  ;; Closures:
  ;; A lambda evaluates to a closure, which stores its variable and body
  ;; PLUS the environment in which it was evaluated, which is captured
  ;; to be used when the function is called
  [closureV (var : Symbol)
        (body : Exp)
        (env : Env)])

;; NEW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments are Association Lists
;; mapping symbols to values
(define-type-alias Env (AssocList Symbol Value))

(define mt-env empty)

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
    ;; Single-argument Lambdas, parse as singleton lists
    [(s-exp-match? `{lam SYMBOL ANY} s)
     (lamS (list (s-exp->symbol (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    ;; Multi-argument lambdas, parse the list of symbols
    [(s-exp-match? `{lam {SYMBOL ...} ANY} s)
     (lamS (map s-exp->symbol (s-exp->list (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    ;; FUnction calls
    ;; Catch-all case for n-ary function calls/applications
    ;; Just parse as a function application: first thing is the function,
    ;; rest are the args, so we map parse to parse each of them
    [(s-exp-match? `{ANY ...} s)
     (appS (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
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
    ;; NEW: we can curry let1 into lambda
    [(let1S var val body)
     (appE (lamE var (desugar body))
           (desugar val))]
    ;; NEW
    ;; Desugar n-ary functions and applications into single ones
    ;; using currying, by folding
    [(lamS xs body)
     (foldr lamE (desugar body) xs)]
    ;; NEW
    ;; Args is a list of expS, so we have to map to desugar each one
    ;; Function applications associate to the left, so we use foldl,
    ;; and we want the function on the left.
    ;; Since appE : (Exp Exp -> Exp) the types don't help us know which to apply first.
    [(appS fun args)
     (foldl (lambda (arg fun) (appE fun arg)) (desugar fun) (map desugar args))]
    ))

;; No need for substitution in the interpreter version

;; Evaluate Expressions
;; NEW
;; Each expression is evaluated in a given environment,
;; which gives values for all the free variables
(define (interp [env : Env]
                [e : Exp]) : Value
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
     (lift-binop + (interp env l) (interp env r))]
    ;; Works the same but for times
    [(timesE l r)
     (lift-binop * (interp env l) (interp env r))]
    ;; {if test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(cndE test thn els)
     (type-case Value (interp env test)
       [(boolV b)
        (if b
            (interp env thn)
            (interp env els))]
       [else (error 'interp "Non-boolean given to if")])]
    ;; To check if value is zero, we interpret it,
    ;; then pattern match on the result
    [(zero?E e)
     (type-case Value (interp env e)
       [(numV n)
        (boolV (= n 0))]
       [else
        (error 'interp "Expected number")])]
    ;; NEW
    ;; For environments, we can evaluate variables by looking them up in the environment.
    ;; So the meaning of a program with variables depends on its environment
    [(varE x)
     (lookup x env)]
    ;; NEW:
    ;; Interpreting functions with environments: we need to
    ;; capture the environment for static scope, so that when we call the function,
    ;; we have values for all of its free variables
    [(lamE var body)
     (closureV var body env)]
    ;; NEW:
    ;; Interpreting function calls (applications)
    ;; We just interpret the body of the function,
    ;; in the environment *from the closure*, extended with
    ;; the concrete value of the argument.
    ;; This is what gives us static scope.
    [(appE fun arg)
     ;; Argument and function are interpreted in the same environment as the whole expression
     ;; e.g. NOT the environment from the closure
     (let* ([argVal (interp env arg)]
            [funVal (interp env fun)])
       (type-case Value funVal
         [(closureV var body funEnv)
          (let* ([envForCall (extend var argVal funEnv)])
          (interp envForCall body))]
         [else
          (error 'interp "Tried to call non-function")]))]))

;; The Language Pipeline
;; We run  program by parsing an s-expression into an expression
;; then interpreting it into a number
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
;; NEW
;; By default, we evaluate expressions in the empty-environment
(define (run s-exp) (interp mt-env (desugar
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

(test (run
       `{let1 {x {+ 9900 99}}
              {+ x {* x {if {zero? x} x {- x x}}}}})
      (numV 9999))

;; Basic lambda test
(test (run
       `{let1 {f {lam x {+ x 3}}}
              {* {f 1} {f 2}}})
      (numV 20))

;; Apply lambda directly
(test (run
       `{let1 {f {lam x {+ x 3}}}
              {* {f 1} {{lam x {- x 7}} 2}}})
      (numV -20))

;; Lambda should be able to refer to variables defined earlier
(test (run
       `{let1 {x 99}
              {let1 {f {lam y {+ x y}}}
                    {f 1}}})
      (numV 100))

;; Lambda should be able to be nested
(test (run
       `{let1 {f {lam x {lam y {+ x y}}}}
              {{f 3} 5}})
      (numV 8))

;; Calling non-function should be error
(test/exn (run `{1 2}) "")

;; Multi-argument lambda tests
(test (run `{let1 {difference {lam {x y}  {- x y}}}
                  {* {difference 3 5} {difference 5 3}}})
      (numV -4))

;; Make sure multi-arg still captures free variables correctly
(test (run `{let1 {x 99}
                  {let1 {f {lam {y z} {* y {+ x z}}}}
                        {f 3 1}}})
      (numV 300))

;; Make sure shadowing works for multi-arg functions
(test (run `{let1 {x 99}
                  {let1 {f {lam {x y z} {* y {+ x z}}}}
                        {f 2 3 1}}})
      (numV 9))

;; We can have 0-argument functions and calls, which just turn into
;; non-function expressions
(test (run `{{+ 33 11}})
      (numV 44))

(test (run `{lam {} {+ 2 3}})
      (numV 5))
