#lang plait


;; Curly-Lambda: A language with first-class functions

;; BNF for Curly-Lambda
;; New forms: {fun {x} body}
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if0" <expr> <expr> <expr> "}"
;;   | "{" "-" <expr> <expr> "}"
;;   | SYMBOL
;;   | "{" "let SYMBOL <expr> <expr> "}"
;;   | "{" SYMBOL <expr> "}"
;;   | NUMBER

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
            [arg : SurfaceExpr])
  ;; Anonymous functions
  (SurfFun [arg : Symbol]
           [body : SurfaceExpr])
  ;; variable definitions
  (SurfLetVar [x : Symbol]
              [xexp : SurfaceExpr]
              [body : SurfaceExpr]))

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
    ;; Same idea as above, but first arg needs to be symbol
    [(s-exp-match? `{letvar SYMBOL ANY ANY} s)
     (SurfLetVar (s-exp->symbol (second (s-exp->list s)))
                 (parse (third (s-exp->list s)))
                 (parse (fourth (s-exp->list s))))]
    ;; Anonymous functions
    [(s-exp-match? `{fun {SYMBOL} ANY} s)
     (SurfFun (s-exp->symbol
               (first (s-exp->list
                       (second (s-exp->list s)))))
              (parse (third (s-exp->list s))))]
    ;; Function calls {f e1}
    ;; Has to come last, only treat as a function call if it's not a binary operator
    [(s-exp-match? `{SYMBOL ANY} s)
     ;;: called function can be any expression
     (SurfCall (parse (first (s-exp->list s)))
               (parse (second (s-exp->list s))))]
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
  ;; Local Varible definitions
  (LetVar [x : Symbol]
          [xexp : Expr]
          [body : Expr])
  ;; anonymous functions
  (Fun [arg : Symbol]
       [body : Expr])
  )

;; NEW: Values
;; Interpreter produces more than just numbers now
;; So we have a datatype of the different kinds of values it can make
(define-type Value
  (ClosureV [arg : Symbol]
            [body : Expr]
            [env : Env])
  (NumV [num : Number]))

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
    [(SurfCall f arg)
     (Call (elab f) (elab arg))]
    ;; NEW: Letvar is desugared in the usual way
    [(SurfLetVar x xexp body)
     (LetVar x (elab xexp) (elab body))]
    [(SurfFun x body)
     (Fun x (elab body))]
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
        [val : Value]))

(test (bind-name (bind 'x (NumV 3))) 'x)
(test (bind-val (bind 'x (NumV 3))) (NumV 3))


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

(test emptyEnv '())
(test (extendEnv (bind 'x (NumV 3)) (extendEnv (bind 'y (NumV 4)) empty))
      (list (bind 'x (NumV 3)) (bind 'y (NumV 4))))



;; Linear search through environments to find a variable
;; Returns error if var not found
;; NEW: returns a Value, not just a Number
;; so we can have functions in the environment
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    ;; Can't find a variable in an empty env
    [empty (error 'lookup (string-append "undefined variable " (to-string n)))]
    ;; Cons: check if the first binding is the var
    ;; we're looking for.
    ;; Return its value  if it is, otherwise
    ;; keep looking in the rest of the list
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-val b)]
                        [else (lookup n rst-env)])]))




;; Evaluate Expressions
;; NEW: (ish): interpret relative to a given context
(define (interp [env : Env]
                [e : Expr] ) : Value
  (begin
    ;;     (display (to-string e))
    ;;     (display "\n------------------------------\n")
    (type-case Expr e
      ;; A number evaluates to itself
      [(NumLit n) (NumV n)]
      ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
      [(Plus l r)
       (liftVal2 + (interp env l) (interp env r))]
      ;; Works the same but for times
      [(Times l r)
       (liftVal2 * (interp env l) (interp env r))]
      ;; {if0 test thn els} evaluates test and checks if it's zero
      ;; if it is, then we evaluate thn
      ;; otherwise we evaluate els
      [(If0 test thn els)
       (if (= 0 (checkAndGetNum (interp env test)))
           (interp env thn)
           (interp env els))]
      ;; We interpret variables by looking them up in the environment
      [(Var x)
       (lookup x env)]
      ;; NEW: Interpreting functions
      ;; This is where we package the function with its environment to build a closure
      [(Fun x body)
       (ClosureV x body env)] ;; Note how we capture Env
      ;; NEW: Function calls
      [(Call funExpr argExpr)
       (let* ([argVal (interp env argExpr)] ;;Evaluate the argument
              [funVal (checkAndGetClosure (interp env funExpr))] ;; Function might be an expression, so have to evaluate
              ;; Get the name, body, and environment from the closure
              [argVar (fst (fst funVal))]  ;; name of the function param
              [funBody (snd (fst funVal))]
              [funEnv (snd funVal)]) ;; body of the function
         ;; Evaluate the body in the extended *closure* environment
         ;; so that we get static scoping 
         (interp (extendEnv (bind argVar argVal) funEnv) ;; <------------
                 funBody))] 
      ;; Interpret a let by interpreting the body in an extended environment
      [(LetVar x xexp body)
       (let ([xval (interp env xexp)])
         (interp (extendEnv (bind x xval) env) body))])))

;; The Language Pipeline
;; We run  program by parsing an s-expression into a surface expression,
;; elaboarting (desugaring) the surface expression into a core expression,
;; then interpreting it
;;
(define (runWithDefs defs s-exp) (interp emptyEnv (elab (parse s-exp))))

;; NEW:
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

;; (define testDefs
;;   (list
;;    (parse-fundef `{define {add5 x} {+ x 5}})
;;    (parse-fundef `{define {double y} {* 2 y}})
;;    (parse-fundef `{define {quadruple x} {double {double x}}})
;;    (parse-fundef `{define {checkIf0 x} {if0 x 1 0}})
;;    (parse-fundef `{define {const0 x} {* 0 {+ 2 {+ 3 {if0 0 {- 3 999999} 40000}}}}})
;;   ;; NEW
;;    (parse-fundef `{define {shadowTriple x} {+ x {letvar x {+ x x} x}}})
;;   ))

;;NEW:
;; For testing, we have run work by interpreting
;; in the context of the above definitions

(define (run s-exp) (interp emptyEnv (elab (parse (withFunctions s-exp)))))

(test (run `3)
      (NumV 3))
(test (run `{+ 1 2})
      (NumV 3))
(test (run `{* 2 {+ 3 5}})
      (NumV 16))

(test (run `{if0 0 1 2})
      (NumV 1))
(test (run `{if0 99 1 2})
      (NumV 2))
(test (run `{if0 {* 2 0} {+ 3 5} {* 3 5}})
      (NumV 8))

(test (run `{+ 3 {if0 3 10 20}})
      (NumV 23))

(test (run `{- 2 1})
      (NumV 1))
(test (run `{* 2 {- 3 5}})
      (NumV -4))

(test (run `{if0 {- 2 2} {- 5 3} {- 3 5}})
      (NumV 2))

(test (run `{if0 {- 2 4} {- 5 3} {- 3 5}})
      (NumV -2))


(test/exn (parse `{1 2}) "invalid input")


(test/exn (run `{+ x 3}) "undefined variable")

(test (run `{if0 0 3 {+ x 3}})
      (NumV 3))

(test (run `{add5 10})
      (NumV 15))

(test (run `{add5 {if0 0 3 4}})
      (NumV 8))

(test (run `{if0 {add5 -5} 3 4})
      (NumV 3))

(test (run `{quadruple 4})
      (NumV 16))

(test (run `{checkIf0 {+ 3 -3}})
      (NumV 1))

(test (run `{const0 3})
      (NumV 0))

(test (run `{letvar x 3 {+ x 10}})
      (NumV 13))

(test (run `{letvar x 3 {letvar x 4 {+ 10 x}}})
      (NumV 14))

;; Good example of shadowing, parameter is x
;; but then defines x to be x + x
;; so you get paramX + (paramX + paramX)
(test (run `{shadowTriple 3}) (NumV 9))

;; Static scoping: Variable x shouldn't enter function scope 
(test (run `{letvar x 22 {shadowTriple 3}}) (NumV 9))



