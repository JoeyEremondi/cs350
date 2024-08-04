#lang plait


;; Curly-A5: A language with curried first-class functions and mutable boxes
;; Four Questions
;; Q1: elaborate surface multi-argument calls into core single-argument calls using a fold
;; Q2: elaborate surface multi-argument functions into core multi-argument functions using a fold
;; Q3: Implement a version of while-loops for Curly-A5
;; Q4: Write a Curly-A5 function that takes in two boxes and swaps the values stored in them.

;; BNF for Curly-Lambda
;; New forms: {fun {x} body}
;; 
;;  <expr> ::=
;;   | SYMBOL
;;   | NUMBER
;;   | "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if0" <expr> <expr> <expr> "}"
;;   | "{" "-" <expr> <expr> "}"
;;   | "{" "let SYMBOL <expr> <expr> "}"
;;   | "{" "fun" "{" SYMBOL * "}" <expr> "}" ;; Can define a function with any number of arguments
;;                                            ;; * indicates "0 or more copies of the preceding thing"
;;   | "{" <expr> <expr>* "}" ;; Can call any expression with any number of argument
;;     NEW:
;;   | "{" "begin" <expr> <expr> "}"
;;   | "{" "box" <expr> "}"
;;   | "{" "unbox" <expr> "}"
;;   | "{" "set-box!" <expr> <expr> "}"


;; BNF for Function definitions
;; <fundef> ::= "{" "define" "{" SYMBOL SYMBOL "}" <expr> "}"

;; Surface AST
;; Represents an expression before we have desugared
;; subtraction away using `elab`
(define-type SurfaceExpr
  ;; A number e.g. 5
  (SurfNumLit [n : Number])
  ;; {+ e1 e2}
  (SurfPlus [left : SurfaceExpr]
            [right : SurfaceExpr])
  ;; {* e1 e2}
  (SurfTimes [left : SurfaceExpr]
             [right : SurfaceExpr])
  ;; {if0 e1 e2 e3}
  (SurfIf0 [test : SurfaceExpr]
           [thenCase : SurfaceExpr]
           [elseCase : SurfaceExpr])
  ;; {- e1 e2}
  ;; This constructor is in SurfExpr but not in Expr
  (SurfSub [left : SurfaceExpr]
           [right : SurfaceExpr])
  ;; Variables (will show up in function definitions)
  (SurfVar [x : Symbol])
  ;;  Function calls
  ;;  Function can be any expression, not just a symbol
  (SurfCall [fun : SurfaceExpr]
            [arg : (Listof SurfaceExpr)])
  ;; Anonymous functions
  (SurfFun [arg : (Listof Symbol)]
           [body : SurfaceExpr])
  ;; variable definitions
  (SurfLetVar [x : Symbol]
              [xexp : SurfaceExpr]
              [body : SurfaceExpr])
  ;; recursive variable definitions
  (SurfLetRec [x : Symbol]
              [xexp : SurfaceExpr]
              [body : SurfaceExpr])
  
  ;; Box creation
  (SurfBox [init : SurfaceExpr])
  ;; Getting a Box's value
  (SurfUnbox [box : SurfaceExpr])
  ;; Setting a Box's value
  (SurfSetbox! [box : SurfaceExpr]
               [newval : SurfaceExpr])
  ;; Do the first expression purely for its side-effects
  ;; then evaluate the second one
  (SurfBegin [e1 : SurfaceExpr]
             [e2 : SurfaceExpr])
  ;; Variable mutation
  (SurfSetvar! [x : Symbol]
               [newval : SurfaceExpr])
  ;; Turn a variable into a Box pointing to the same location
  (SurfGetLoc [x : Symbol]))

;; Parse
;; Takes an S-expression and turns it into an SurfExpr
;; Raises an error if it doesn't represent a valid program
(define (parse [s : S-Exp]) : SurfaceExpr
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (SurfNumLit (s-exp->number s))]
    ;; x (variable)
    [(s-exp-match? `SYMBOL s) (SurfVar (s-exp->symbol s))]
    ;; {+ s1 s2}
    [(s-exp-match? `{+ ANY ANY} s)
     (SurfPlus (parse (second (s-exp->list s)))
               (parse (third (s-exp->list s))))]
    ;; {* s1 s2}
    [(s-exp-match? `{* ANY ANY} s)
     (SurfTimes (parse (second (s-exp->list s)))
                (parse (third (s-exp->list s))))]
    ;; Same idea as above, but we're looking for 3 arguments, not 2, for if0
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (SurfIf0 (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s)))
              (parse (fourth (s-exp->list s))))]
    ;; {- s1 s2}
    [(s-exp-match? `{- ANY ANY} s)
     (SurfSub (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    ;; {box e}
    [(s-exp-match? `{box ANY} s)
     (SurfBox (parse (second (s-exp->list s))))]
    ;; {unbox e}
    [(s-exp-match? `{unbox ANY} s)
     (SurfUnbox (parse (second (s-exp->list s))))]
    ;; {set-box! e1 e2}
    [(s-exp-match? `{set-box! ANY ANY} s)
     (SurfSetbox! (parse (second (s-exp->list s)))
                  (parse (third (s-exp->list s))))]
    ;; {set-var! e1 e2}
    [(s-exp-match? `{set-var! SYMBOL ANY} s)
     (SurfSetvar! (s-exp->symbol (second (s-exp->list s)))
                  (parse (third (s-exp->list s))))]
    ;; {get-loc x}
    [(s-exp-match? `{get-loc SYMBOL} s)
     (SurfGetLoc (s-exp->symbol (second (s-exp->list s))))]                  
    ;; {begin e1 e2}
    [(s-exp-match? `{begin ANY ANY} s)
     (SurfBegin (parse (second (s-exp->list s)))
                (parse (third (s-exp->list s))))]
    ;; Same idea as above, but first arg needs to be symbol
    [(s-exp-match? `{letvar SYMBOL ANY ANY} s)
     (SurfLetVar (s-exp->symbol (second (s-exp->list s)))
                 (parse (third (s-exp->list s)))
                 (parse (fourth (s-exp->list s))))]
    ;; Same idea as above, but first arg needs to be symbol
    [(s-exp-match? `{letrec SYMBOL ANY ANY} s)
     (SurfLetRec (s-exp->symbol (second (s-exp->list s)))
                 (parse (third (s-exp->list s)))
                 (parse (fourth (s-exp->list s))))]
    ;; Anonymous functions
    [(s-exp-match? `{fun {SYMBOL ...} ANY} s)
     (SurfFun (map s-exp->symbol
                   (s-exp->list
                    (second (s-exp->list s))))
              (parse (third (s-exp->list s))))]
    ;; Function calls {f e1}
    ;; Has to come last, only treat as a function call if it's not a binary operator
    [(s-exp-match? `{ANY ANY ...} s)
     ;;: called function can be any expression
     (SurfCall (parse (first (s-exp->list s)))
               (map parse (rest (s-exp->list s))))]
    
    [else (error 'parse "invalid input")]))


;; Abstract syntax for Curly-Sub
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
  ;; {if0 e1 e2 e3}
  (If0 [test : Expr]
       [thenCase : Expr]
       [elseCase : Expr])
  ;; Variables (will show up in function definitions)
  (Var [x : Symbol])
  ;; Function calls
  ;; function can be any name, not just a symbol
  (Call [fun : Expr]
        [arg : Expr])
  ;; anonymous functions
  (Fun [arg : Symbol]
       [body : Expr])
  ;; variable definition
  (LetVar [x : Symbol]
          [xexpr : Expr]
          [body : Expr])
  ;; recursive variable definition
  (LetRec [x : Symbol]
          [xexpr : Expr]
          [body : Expr])
  ;; Box creation
  (Box [init : Expr])
  ;; Getting a Box's value
  (Unbox [box : Expr])
  ;; Setting a Box's value
  (Setbox! [box : Expr]
           [newval : Expr])
  ;; Begin
  (Begin [l : Expr]
         [r : Expr])
  ;; Variable mutation
  (Setvar! [x : Symbol]
           [val : Expr])
  ;; Turn a variable into a Box
  ;; pointing to the same location
  (GetLoc [x : Symbol])
  )

;; NEW: Values
;; Interpreter produces more than just numbers now
;; So we have a datatype of the different kinds of values it can make
(define-type Value
  (ClosureV [arg : Symbol]
            [body : Expr]
            [env : Env])
  (NumV [num : Number])
  ;; A box is represented as a location in the store
  (BoxV [loc : Location])
  ;; NEW
  ;; Placeholder for implementing recursion
  ;; If we ever produce this, we should get an error
  (DummyV))

(define (helper-SurfCall [args : (Listof SurfaceExpr)]
                         [accum : Expr]) : Expr
  (type-case (Listof SurfaceExpr) args
    [empty accum]
    [(cons arg rest)
     (helper-SurfCall rest (Call accum (elab arg)))]))

;; Elaborate (desugar) surface expressions into
;; core expressions
;; For every case except for Sub, there's a core constructor
;; that takes the exact same arguments as the surface constructor
;; (except that its sub-expressions are core, not surface)
;; so we can just apply the corresponding Core constructor and apply elab recursively
;; Exact same as substitution version
(define (elab [expr : SurfaceExpr]) : Expr
  (type-case SurfaceExpr expr
    ;; Trivial cases: expressions that are the same in the surface and core syntax
    [(SurfNumLit n) (NumLit n)]
    ;; For plus, times, and if, we need to recursively elaborate the sub-expressions
    ;; because they might contain a subtraction
    [(SurfPlus l r) (Plus (elab l) (elab r))]
    [(SurfTimes l r) (Times (elab l) (elab r))]
    [(SurfIf0 test thn els) (If0 (elab test) (elab thn) (elab els))]
    ;; {- e1 e2} becomes {+ e1 {* -1 e2}}
    [(SurfSub l r)
     (Plus (elab l) (Times (elab r) (NumLit -1)))]
    ;; variables are core expressions, so there's nothing to do for desugaring
    [(SurfVar x) (Var x)]
    ;; NEW
    ;; Use recursion to desugar multi-argument function calls
    ;; into single argument calls using Currying.

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    [(SurfCall f args)
     (helper-SurfCall args (elab f))]
    
    [(SurfLetVar x xexp body)
     (LetVar x (elab xexp) (elab body))]
    [(SurfLetRec x xexp body)
     (LetRec x (elab xexp) (elab body))]
    ;; NEW:
    ;; SurfFun now takes a list of argument variables.
    ;; Use recursion to desugar mutli-parameter functions
    ;; into single parameter functions using Currying
    [(SurfFun xs body)
     (type-case (Listof Symbol) xs
       [empty (elab body)]
       [(cons x rest)
        (Fun x (elab (SurfFun rest body)))])]
    ;; Box operations just desugar in the usual way
    [(SurfBox e)
     (Box (elab e))]
    [(SurfUnbox e)
     (Unbox (elab e))]
    [(SurfSetbox! e1 e2)
     (Setbox! (elab e1) (elab e2))]
    [(SurfBegin e1 e2)
     (Begin (elab e1) (elab e2))]
    [(SurfSetvar! x e)
     (Setvar! x (elab e))]
    [(SurfGetLoc x)
     (GetLoc x)]
    ))




;;;;;;;;;;;;;;;;;;;
;; helper functions for dynamic type checking.
;; Check if a value is a number, and get the number if it is
(define (checkAndGetNum [v : Value]) : Number
  (type-case Value v
    [(NumV n) n]
    [else
     (error 'curlyTypeError
            (string-append "Expected Number, got function:"
                           (to-string v)))]))

;;NEW
;; Check if a value is a closure, and get its parameter, body and environment if it is
(define (checkAndGetClosure [v : Value]) : ((Symbol * Expr) * Env)
  (type-case Value v
    [(ClosureV x body env)
     (pair (pair x body) env)]
    [else
     (error 'curlyTypeError
            (string-append "Expected Function, got number:"
                           (to-string v)))]))

;; Lift a binary operation on Numbers to Values
(define (liftVal2 [f : (Number Number -> Number)]
                  [x : Value]
                  [y : Value]) : Value
  (let ([nx (checkAndGetNum x)]
        [ny (checkAndGetNum y)])
    (NumV (f nx ny))))


; Pairs of variable and values
;; NEW: Value datatype, not just number, in a binding
(define-type Binding
  (bind [name : Symbol]
        [loc : Location]))



;; Lets us write Env instead of (Listof Binding)
;; But it's not defining a new type,
;; just a new name for the same type.
(define-type-alias Env (Listof Binding))

;; Environment is either empty or extended env
(define emptyEnv : Env
  empty)
(define (extendEnv [bnd : Binding]
                   [env : Env])
  : Env
  (cons bnd env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Store data structure and interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NEW Locations
;; an abstract version of memory addresses
(define-type-alias Location Number)

;; NEW: Store locations
(define-type Storage
  (cell [location : Location] 
        [val : Value]))

;; A store is a list of location-value pairs
;; Basically a very inneficient implementation of dictionaries
(define-type-alias Store (Listof Storage))
(define mt-store empty)

;; Create a new store that's the same as the old one
;; but with the given value at the given location
(define override-store : (Storage Store -> Store)
  cons)

;;NEW: Get a location not contained in the given store
(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

;; Helper for new-loc
(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
    [empty 0]
    [(cons c rst-sto) (max (cell-location c)
                           (max-address rst-sto))]))

;; NEW:
;; Get the value at a particular location in a store,
;; and return an error if it's not in the store.
(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
    [empty (error 'interp "unallocated location")]
    [(cons c rst-sto) (if (equal? l (cell-location c))
                          (cell-val c)
                          (fetch l rst-sto))]))



;; NEW: A value-store pair that interp produces
(define-type Result
  (v*s [v : Value] [s : Store]))

;; NEW: 
;; with form ----------------------------------------
;; Racket lets us define our own syntactic sugar
;; So we can interpret an expression and get both
;; the name and value that are returned
(define-syntax-rule
  (with [(v-id sto-id) call]
        body)
  (type-case Result call
    [(v*s v-id sto-id) body]))


;; Linear search through environments to find a variable
;; Returns error if var not found
;; NEW: returns a Value, not just a Number
;; so we can have functions in the environment
(define (lookup [n : Symbol] [env : Env]) : Location
  (type-case (Listof Binding) env
    ;; Can't find a variable in an empty env
    [empty (error 'lookup (string-append "undefined variable " (to-string n)))]
    ;; Cons: check if the first binding is the var
    ;; we're looking for.
    ;; Return its value  if it is, otherwise
    ;; keep looking in the rest of the list
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-loc b)]
                        [else (lookup n rst-env)])]))




;; Evaluate Expressions
;; NEW: (ish): interpret relative to a given context
(define (interp [env : Env]
                [e : Expr]
                [sto : Store]) : Result
  (type-case Expr e
    ;; A number evaluates to itself
    [(NumLit n)
     (v*s (NumV n) sto)]
    ;; {+ e1 e2} evaluates e1, giving a value and a store.
    ;; It then evaluates e2 in that new store, giving a value and a final store.
    ;; The final store is the result of
    ;; Except for the box operations, all other expressions cases follow this pattern for state:
    ;; interpret one sub argument, get a store, then use that to interpret the next sub-argument
    [(Plus l r)
     (with [(v-l sto-l) (interp env l sto)]
           (with [(v-r sto-r) (interp env r sto-l)]
                 (v*s (liftVal2 + v-l v-r) sto-r)))]
    ;; Works the same but for times
    [(Times l r)
     (with [(v-l sto-l) (interp env l sto)]
           (with [(v-r sto-r) (interp env r sto-l)]
                 (v*s (liftVal2 * v-l v-r) sto-r)))]
    ;; {if0 test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    ;; Note that we have to evaluate the branch in the store from evaluating the test.
    ;; We only evaluate the branch we take, so the state changes from the other branch are never done.
    ;; Also note that this evaluating the branch is a tail call.
    [(If0 test thn els)
     (with ([v-test sto-test] (interp env test sto))
           (if (= 0 (checkAndGetNum v-test))
               (interp env thn sto-test)
               (interp env els sto-test)))]
    ;; We interpret variables by looking them up in the environment
    ;; Variables don't change or the state at all
    ;; since all mutation is done behind boxes.
    [(Var x)
     (v*s (fetch (lookup x env) sto) sto)]
    ;; Interpreting functions
    ;; This is where we package the function with its environment to build a closure.
    ;; No changes to the store, since we're not actually running the function body
    [(Fun x body)
     (v*s (ClosureV x body env) sto)] ;; Note how we capture Env
    ;; NEW: Function calls
    [(Call funExpr argExpr)
     (with ([fun-v fun-sto] (interp env funExpr sto))
           (with ([arg-v arg-sto] (interp env argExpr fun-sto))
                 (let* (
                        [funPair (checkAndGetClosure fun-v)] ;; Function might be an expression, so have to evaluate
                        [argVar (fst (fst funPair))]
                        [funBody (snd (fst funPair))]
                        [funEnv (snd funPair)]
                        ;; Allocate a new location for the argument value
                        [argLoc (new-loc arg-sto)]
                        ;; new store with the arg value at the new location
                        ;; Use most recent store from arg
                        [body-sto (override-store (cell argLoc arg-v) arg-sto)])
                   ;; Evaluate the body in the extended *closure* env
                   ;; with the new location bound to the parameter name,
                   ;; using the new store with the argument value
                   (interp (extendEnv (bind argVar argLoc) funEnv)
                           funBody
                           body-sto))))]

    [(LetVar x xexpr body)
     (with ([x-val x-sto] (interp env xexpr sto))
           (let* ([xloc (new-loc x-sto)]
                  [body-sto (override-store (cell xloc x-val) x-sto)])
             (interp (extendEnv (bind x xloc) env) body body-sto)))]
    [(Box a)
     ;; Get the value we're putting in the box, and the new memory state 
     (with [(v sto-v) (interp env a sto)]
           ;; allocate a new location in memory
           (let ([l (new-loc sto-v)])
             ;; Return the new location, wrapped as a Value using BoxV
             ;; along with the new memory, that has the new location+value added
             (v*s (BoxV l)
                  (override-store (cell l v)
                                  sto-v))))]
    [(Unbox a)
     ;; Evaluate the box we're looking up 
     (with [(v sto-v) (interp env a sto)]
           ;; If it's a Box value, then use fetch to get the value from the location in memory
           ;; returning that value and the new state from interpreting the box
           (type-case Value v
             [(BoxV l) (v*s (fetch l sto-v)
                            sto-v)]
             ;; Otherwise type error
             [else (error 'interp "not a box")]))]
    [(Setbox! bx val)
     ;; Interpret both the box we're assigning to and the value we're putting in it,
     ;; being careful to use the new memory state from the first when computing the second.
     
     (with [(v-b sto-b) (interp env bx sto)]
           (with [(v-v sto-v) (interp env val sto-b)]
                 (type-case Value v-b
                   [(BoxV l)
                    ;; If the first expression was actually a box,
                    ;; return the value, with a new memory state
                    ;; that has the new value (v-v) at the box's location (l)
                    (v*s v-v
                         (override-store (cell l v-v)
                                         sto-v))]
                   ;; Otherwise type error
                   [else (error 'interp "not a box")])))]
    [(Begin l r)
     (with ([v-l sto-l] (interp env l sto))
           (interp env r sto-l))]
    ;; Like set-box! but we get the location from the environment
    ;; instead of evaluating a box-expression
    [(Setvar! x e)
     (with ([e-val e-sto] (interp env e sto))
           (v*s e-val
                ;; Get the location from the environment
                (override-store (cell (lookup x env) e-val)
                                e-sto)))]
    [(GetLoc x)
     ;; Make a new box with the same location
     ;; that the current environment has for the variable
     (v*s (BoxV (lookup x env))
          ;; No changes to the store
          sto)]
    [(LetRec x xexpr body)
     (let* ([x-loc (new-loc sto)] ;; Location for x
            [dummy-sto ;; Put a dummy value at x's location
             (override-store (cell x-loc
                                   (DummyV)) sto)])
       (with ([x-val x-sto]
              ;; Interpret xexpr in env with x's location
              (interp (extendEnv (bind x x-loc) env)
                      xexpr
                      dummy-sto))
             ;; Interpret body in env with x's location
             ;; and store with x's newly computed value
             ;; plus any side-effects from xexpr
             (interp (extendEnv (bind x x-loc) env) body
                     (override-store (cell x-loc x-val) x-sto))))]



    )) 



;; A list of helpful function definitions
;; we can use while testing
;; This is a function that takes an s-expression and produces another s-expression
;; using slicing, but you don't need to worry how it works.

(define (withFunctions s-expr)
  `{letvar add5 {fun {x} {+ x 5}}
           {letvar double {fun {y} {* 2 y}}
                   {letvar quadruple {fun {x} {double {double x}}}
                           {letvar checkIf0 {fun {x} {if0 x 1 0}}
                                   {letvar const0 {fun {x} {* 0 {+ 2 {+ 3 {if0 0 {- 3 999999} 40000}}}}}
                                           {letvar shadowTriple {fun {x} {+ x {letvar x {+ x x} x}}} ,s-expr}
                                           }}}}})



;; The Language Pipeline
;; For testing, we have run work by interpreting
;; in the context of the above definitions
(define (run s-exp) (v*s-v (interp emptyEnv (elab (parse s-exp)) '())))

;;Like run but has helper functions defined as above
(define (runWithDefs s-exp) (v*s-v (interp emptyEnv (elab (parse (withFunctions s-exp))) '())))


;; Test multiple function creation
(test (run `{letvar f {fun {x y z} {+ x {+ y z}}}
                    0})
      (NumV 0))

;; ;; Test multiple function creation
(test (run `{letvar f {fun {f} {f 1 2 3}}
                    0})
      (NumV 0))

;; Test calling a curried function
;; If you're getting 65 you're probably using the wrong fold somewhere
(test (run `{letvar f {fun {x y z} {* x {+ y z}}}
                    {f 10 3 5}})
      (NumV 80))

;; Test partially applying a function.
;; Since a multiple argument function is desugared into nested lambdas,
;; we can call it with fewer than its full slate of arguments,
;; and get a function still waiting for more arguments.
;; If this is wrong, you're maybe using the wrong fold somewhere.
(test (run `{letvar f {fun {x y z} {* x {+ y z}}}
                    {letvar f10 {f 10}
                            {- {f10 3 5} {f10 1 2}}}})
      (NumV 50))

(test (run `{letvar x {box 3}
                    {letvar y x
                            {begin {set-box! y 10}
                                   {unbox x}}}})
      (NumV 10)
      )

;; Test to make sure we have pass-by-value
(test (run `{letvar y 2
                    {letvar f {fun {x} {begin
                                         {set-var! x {* x 3}}
                                         x}}
                            {+ {f y} y}}})
      (NumV 8))

;; Variable reference capturing
;; to simulate call-by-reference
(test (run `{letvar doublebox! {fun {x} {set-box! x {* 2 {unbox x}}}}
                    {letvar y 3
                            {begin {doublebox! {get-loc y}}
                                   y}}}
           ) (NumV 6))

(test (run `{letrec fact {fun {x}
                  {if0 x
                       1
                       {* x {fact {- x 1}}}}}
        {fact 5}})
      (NumV 120))
