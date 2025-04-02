#lang flit

;; Curly-Mutvar
;; This language extends the store-passing interpreter of Curly-Box
;; to treat variables as mutable: every variable defined using let1 or lam
;; can have its value changed in the store, using set-var!
;; We can also create a box that points to a given variable's location
;; using address-of.


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
;;   | {seq <expr> <expr} ;; Run one expression, discard its result, and evaluate the second one
;;                        ;; only makes sense with side-effects
;;   | {box <expr>} ;; Make a new box that points to a location in memory, whose initial value
;;                      is given by the given expression
;;   | {unbox <expr>} ;; Get the value in memory for a given box's location.
;;   | {set-box! <expr> <expr>} ;; Overwrite the value at the location that the first expression
;;                              ;; points to, using the value from the second expression
;;    ;; NEW
;;   | {set-var! VARIABLE <expr>} ;; Overwite the value of the given variable
;;                                 ;; to the given expressions's value
;;   | {address-of VARIABLE} ;; Produce a box whose location is the given variable's location





;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for association lists
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments are association lists,
;; e.g. symbol-Location pairs, that we can
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
        (error 'find (string-append
                      "Didn't find key "
                      (string-append (to-string k)
                                     " in list")))
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
  ;; Surface language has n-ary function calls and lambdas
  ;; These get desugared away
  (lamS [var : (Listof Symbol)]
        [body : ExpS])
  (appS [fun : ExpS] [args : (Listof ExpS)])
  ;; Sequencing of operations (possibly with side-effects)
  ;; and box operations
  (seqS [l : ExpS]
        [r : ExpS])
  (boxS [e : ExpS])
  (unboxS [e : ExpS])
  (set-box!S [ebox : ExpS]
             [eval : ExpS])
  ;; NEW
  ;; We can set variable values and get their locations
  (set-var!S [var : Symbol]
             [e : ExpS])
  ;; Get the location for a particular variable
  (address-ofS [var : Symbol]))

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
  ;; Sequencing of operations (possibly with side-effects)
  ;; and box operations
  (seqE [l : Exp]
        [r : Exp])
  (boxE [e : Exp])
  (unboxE [e : Exp])
  (set-box!E [ebox : Exp]
             [eval : Exp])
  ;; NEW
  ;; We can set variable values and get their locations
  (set-var!E [var : Symbol]
             [e : Exp])
  ;; Get the location for a particular variable
  (address-ofE [var : Symbol])
  )

;; We now allow values to be either Numbers, Booleans, or Functions
(define-type Value
  [numV (n : Number)]
  [boolV (b : Boolean)]
  ;; Closures:
  ;; A lambda evaluates to a closure, which stores its variable and body
  ;; PLUS the environment in which it was evaluated, which is captured
  ;; to be used when the function is called
  [closureV (var : Symbol)
            (body : Exp)
            (env : Env)]

  ;; Boxes let us treat memory locations
  ;; as values in our language
  [boxV [loc : Location]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stores are association lists
;; Mapping locations to Values
(define-type-alias Location Number)

(define-type-alias Store (AssocList Number Value))

(define mt-store : Store
  empty)
(define override-store
  : (Location Value Store -> Store)
  extend)

;; Code to find the largest index in the store,
;; so we can use one greater as a fresh location
(define (new-loc [sto : Store])
  : Location
  (+ 1 (max-loc sto)))

;; Helper for getting the max location in a store
(define (max-loc [sto : Store])
  : Location
  (foldl max 0 (map pairFst sto)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW : Environments map symbols to locations, not values
;;  so that we can have mutable variables
(define-type-alias Env (AssocList Symbol Location))

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
    ;; Box operations and sequencing
    [(s-exp-match? `{seq ANY ANY} s)
     (seqS (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxS (parse (second (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxS (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (set-box!S (parse (second (s-exp->list s)))
                (parse (third (s-exp->list s))))]
    ;; NEW
    ;; Mutable variables
    [(s-exp-match? `{set-var! SYMBOL ANY} s)
     (set-var!S (s-exp->symbol (second (s-exp->list s)))
                (parse (third (s-exp->list s))))]
    [(s-exp-match? `{address-of SYMBOL} s)
     (address-ofS (s-exp->symbol (second (s-exp->list s))))]
    ;; Function calls
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
    ;; we can curry let1 into lambda
    [(let1S var val body)
     (appE (lamE var (desugar body))
           (desugar val))]
    ;; Desugar n-ary functions and applications into single ones
    ;; using currying, by folding
    [(lamS xs body)
     (foldr lamE (desugar body) xs)]
    ;; Args is a list of expS, so we have to map to desugar each one
    ;; Function applications associate to the left, so we use foldl,
    ;; and we want the function on the left.
    ;; Since appE : (Exp Exp -> Exp) the types don't help us know which to apply first.
    [(appS fun args)
     (foldl (lambda (arg fun) (appE fun arg)) (desugar fun) (map desugar args))]
    ;; No interesting desugaring for boxes, just desugar the parts
    [(seqS l r)
     (seqE (desugar l) (desugar r))]
    [(boxS e)
     (boxE (desugar e))]
    [(unboxS e)
     (unboxE (desugar e))]
    [(set-box!S ebox evalue)
     (set-box!E (desugar ebox)
                (desugar evalue))]
    ;; NEW
    ;; Nothing interesting to desugar, just desugar the parts
    ;; for mutable variables
    [(set-var!S var e)
     (set-var!E var (desugar e))]
    [(address-ofS var)
     (address-ofE var)]
    ))

;; Our interpreter now produces a Value-Store pair
(define-type-alias Result
  (Value * Store))

(define (v*s [v : Value]
             [s : Store])
  : Result
  (pair v s))

;; Macro for pattern matching on results/values
;; Makes the code for interp more readable
(define-syntax with
  (syntax-rules ()
    [(with body) body]
    [(with [(v-id sto-id) call] [(v-ids sto-ids) calls]... body)
     (let* ([vs call]
            [v-id (pairFst vs)]
            [sto-id (pairSnd vs)])
       (with [(v-ids sto-ids) calls]... body))]))




;; Evaluate Expressions
;; Each expression is evaluated in a given environment,
;; which gives values for all the free variables
(define (interp [env : Env]
                [e : Exp]
                [sto : Store]) : Result 
  (type-case Exp e
    ;; A number evaluates to itself
    ;; Need to wrap in numV, since result type is value
    [(numE n)
     (v*s (numV n)
          sto)]
    ;; A boolean evaluates to itself
    [(boolE b)
     (v*s (boolV b)
          sto)]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    ;; e2 is evaluated in the store that results from evaluating e1.
    ;; The other expressions are similar
    [(plusE l r)
     (with [(v-l sto-l) (interp env l sto)]
           ;; Use the store result from the left to interpret the right
           [(v-r sto-r) (interp env r sto-l)]
           ;; Use the store from the right as our result store
           (v*s (lift-binop + v-l v-r)
                sto-r))]
    ;; Works the same but for times
    [(timesE l r)
     (with [(v-l sto-l) (interp env l sto)]
           ;; Use the store result from the left to interpret the right
           [(v-r sto-r) (interp env r sto-l)]
           ;; Use the store from the right as our result store
           (v*s (lift-binop * v-l v-r)
                sto-r))]
    ;; {if test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(cndE test thn els)
     (with [(test-v test-sto) (interp env test sto)]
           (type-case Value test-v
             [(boolV b)
              (if b
                  (interp env thn test-sto)
                  (interp env els test-sto))]
             [else (error 'interp "Non-boolean given to if")]))]
    ;; To check if value is zero, we interpret it,
    ;; then pattern match on the result
    [(zero?E e)
     (with [(e-v e-sto) (interp env e sto)]
           (type-case Value e-v
             [(numV n)
              (v*s (boolV (= n 0))
                   e-sto)]
             [else
              (error 'interp "Expected number")]))]
    ;; NEW:
    ;; If variables are mutable, then the environment stores locations, not values,
    ;; so after looking a symbol up in the environment, we have to get its value from the Store.
    [(varE x)
     (v*s (let* ([xloc (lookup x env)]
                 [xvalue (lookup xloc sto)])
            xvalue)
          sto)]
    ;; Interpreting functions with environments: we need to
    ;; capture the environment for static scope, so that when we call the function,
    ;; we have values for all of its free variables
    [(lamE var body)
     (v*s (closureV var body env)
          sto)]
    ;; Interpreting function calls (applications)
    ;; We just interpret the body of the function,
    ;; in the environment *from the closure*, extended with
    ;; the concrete value of the argument.
    ;; This is what gives us static scope.
    [(appE fun arg)
     ;; Argument and function are interpreted in the same environment as the whole expression
     ;; e.g. NOT the environment from the closure.
     ;; NEW: When we call a function, we need to allocate a new store location for its parameter
     (with [(argVal arg-sto) (interp env arg sto)]
           [(funVal fun-sto) (interp env fun arg-sto)]
           (type-case Value funVal
             [(closureV var body funEnv)
              ;; Allocate a new location for the function parameter
              ;; This is what makes it pass by value: the argument value
              ;; is copied to a new location.
              (let* ([paramLoc (new-loc fun-sto)]
                     ;; New environment has the function variable with the new location
                     [envForCall (extend var paramLoc funEnv)]
                     ;; New store has the argument value at the new location
                     [stoForCall (override-store paramLoc argVal fun-sto)])
                ;;Run the function body in the new env, with the argument in the store
                (interp envForCall body stoForCall))]
             [else
              (error 'interp "Tried to call non-function")]))]
    ;; To sequence two expressions, evaluate the first
    ;; to get the resulting store.
    ;; Then we evaluate the second in that store.
    [(seqE l r)
     (with [(v-l sto-l) (interp env l sto)]
           (interp env r sto-l))]
    ;; To evaluate boxing of an expression,
    ;; we evaluate the expression to get its value
    ;; and the resulting store.
    ;; We find an unused memory location in that store,
    ;; and make a new store where the expression's value
    ;; is at the new location.
    ;; We then use that store, and return a new box value
    ;; that points to the newly created location.
    [(boxE a)
     (with [(v sto-v) (interp env a sto)]
           (let ([loc (new-loc sto-v)])
             (v*s (boxV loc)
                  (override-store loc v
                                  sto-v))))]
    ;; To unbox an expression, we evaluate it to a value/store
    ;; Then we do a dynamic type check to ensure it's a boxV.
    ;; If it is, we return the value that is at that location in the store.
    [(unboxE a)
     (with [(v sto-v) (interp env a sto)]
           (type-case Value v
             [(boxV l) (v*s (lookup l sto-v)
                            sto-v)]
             [else (error 'interp "not a box")]))]

    ;; To overwrite the value at a given location,
    ;; we evaluate the box-expression to a value/store,
    ;; then use that store to evaluate the new value's expression to a value/store.
    ;; We do a dynamic type check that the box's value is a boxV, and if it is,
    ;; we override that location in the store with the new value.
    ;; We then return the new value, in the new store with the updated value.
    [(set-box!E bx val)
     (with [(v-b sto-b) (interp env bx sto)]
           [(v-v sto-v) (interp env val sto-b)]
           (type-case Value v-b
             [(boxV loc)
              (v*s v-v
                   (override-store loc v-v
                                   sto-v))]
             [else (error 'interp "not a box")]))]
    ;; NEW
    ;; Just like set-box, but instead of writing to a box's location,
    ;; we write to the location of a variable in the environment
    [(set-var!E var e)
     (with [(v-v sto-v) (interp env e sto)]
           (v*s v-v
                ;; Get the location to write from the environment
                (override-store (lookup var env) v-v
                                sto-v)))]
    ;; NEW
    ;; We can get a reference to a variable as a box,
    ;; so we can access variables by reference without implementing
    ;; full call-by-reference semantics
    [(address-ofE var)
     (v*s (boxV (lookup var env))
          sto)]

    ))

;; The Language Pipeline
;; We run  program by parsing an s-expression into an expression
;; then interpreting it into a number
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
;; NEW
;; By default, we evaluate expressions in the empty-environment
(define (run s-exp) (pairFst
                     (interp mt-env (desugar
                                     (parse s-exp))
                             mt-store)))

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

;; Box tests
;; Taken from the examples in the slides

;; Copying a box just copies the location
;; so x and y point to the same place in the store
(test (run `{let1 {x {box 3}}
                  {let1 {y x}
                        {seq {set-box! y 10}
                             {unbox x}}}}
           )
      (numV 10))

;; Functions that work on boxes
;; Side effects are seen after the function call
(test (run `{let1 {double!
                   {lam x
                        {set-box! x
                                  {* 2 {unbox x}}}}}
                  {let1 {b {box 3}}
                        {seq {double! b}
                             {unbox b}}}})
      (numV 6))

;; The order we evaluate + args matter,
;; since there might be side-effects
(test (run `{let1 {b {box 3}}
                  {+ {seq {set-box! b {* 3 {unbox b}}}
                          5}
                     {unbox b}}} )
      (numV 14))

;; NEW
;; Tests for mutable variables
(test (run `{let1 {x 3}
                  {seq {set-var! x {* x 2}}
                       x}})
      (numV 6))

;; Make sure have pass-by-value semantics
(test (run `{let1 {y 2}
                  {let1 {f {lam x {seq
                                   {set-var! x {* x 3}}
                                   x}}}
                        {+ {f y} y}}})
      (numV 8))

;; Make sure address-of properly aliases
;; boxes and variables
(test (run `{let1 {doublebox! {lam x {set-box! x {* 2 {unbox x}}}}}
                  {let1 {y 3}
                        {seq {doublebox! {address-of y}}
                             y}}})
      (numV 6))