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
;;   | "{" <expr> <expr> "}" ;;NEW: functions can be any expression now
;;   | NUMBER

;; BNF for Function definitions
;; <fundef> ::= "{" "define" "{" SYMBOL SYMBOL "}" <expr> "}"

;; Surface AST
;; Represents a Curly-Sub expression before we have desugared
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
  ;; NEW: Function can be any expression, not just a symbol
  (SurfCall [fun : SurfaceExpr]
            [arg : SurfaceExpr])
  ;;NEW: Anonymous functions
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
    ;; NEW: Anonymous functions
    [(s-exp-match? `{fun {SYMBOL} ANY} s)
     (SurfFun (s-exp->symbol
               (first (s-exp->list
                       (second (s-exp->list s)))))
              (parse (third (s-exp->list s))))]
    ;; Function calls {f e1}
    ;; Has to come last, only treat as a function call if it's not a binary operator
    [(s-exp-match? `{SYMBOL ANY} s)
     ;; NEW: called function can be any expression
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
  ;; NEW: function can be any expression, not just a symbol
  (Call [fun : Expr]
        [arg : Expr])
  ;; Local Varible definitions
  (LetVar [x : Symbol]
          [xexp : Expr]
          [body : Expr])
  ;; NEW: anonymous functions
  (Fun [arg : Symbol]
       [body : Expr])
  )

;; NEW: Values
;; Interpreter produces more than just numbers now
;; So we have a datatype of the different kinds of values it can make
(define-type Value
  (FunV [arg : Symbol]
        [body : Expr])
  (NumV [num : Number]))

;; Elaborate (desugar) surface expressions into
;; core expressions
;; For every case except for Sub, there's a core constructor
;; that takes the exact same arguments as the surface constructor
;; (except that its sub-expressions are core, not surface)
;; so we can just apply the corresponding Core constructor and apply elab recursively
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



;; Apply a substitution replacing x with expression e1 inside expression e2
(define (subst [toReplace : Symbol]
               [replacedBy : Expr]
               [replaceIn : Expr]) : Expr
  (type-case Expr replaceIn
    ;; Base case
    ;; No variables to replace in a number
    [(NumLit n)
     (NumLit n)]
    ;; Replacing a variable if an expression is just one variable:
    ;; Check if it's the variable we're trying to replace.
    ;; If it is, produce the expression we're replacing it by.
    ;; Otherwise, ignore it and replace the different variable.
    ;; This will be more useful when we have multi-argument functions.
    [(Var x)
     (if (equal? x toReplace)
         replacedBy
         (Var x))]
    ;; Every other case: we have a constructor applied to some expressions
    ;; so we replace the variable in that expression by replacing it in each sub-expression
    ;; and wrapping them back up in whatever constructor we started with.
    [(Plus l r)
     (Plus (subst toReplace replacedBy l)
           (subst toReplace replacedBy r))]
    [(Times l r)
     (Times (subst toReplace replacedBy l)
            (subst toReplace replacedBy r))]
    [(If0 test thn els)
     (If0 (subst toReplace replacedBy test)
          (subst toReplace replacedBy thn)
          (subst toReplace replacedBy els))]
    ;; NEW
    ;; We can't replace a function name, since the Expr AST requires that the function name be
    ;; a symbol, not an expression. So we just recursively replace in the argument.
    [(Call funExpr arg)
     (Call (subst toReplace replacedBy funExpr)
           (subst toReplace replacedBy arg))]
    ;; Don't substitute if the variable is shadowed
    [(LetVar x xexp body)
     (LetVar x
             (subst toReplace replacedBy xexp)
             (if (symbol=? x toReplace)
                 body
                 (subst toReplace replacedBy body)))]
    ;;NEW
    ;; Substitution in a function body
    [(Fun x body)
     ;; Don't substitute if variable is shadowed
     (if (symbol=? x toReplace)
         (Fun x body)
         (Fun x (subst toReplace replacedBy body)))]
    ))


;;;;;;;;;;;;;;;;;;;
;; NEW: helper functions for dynamic type checking.
;; Check if a value is a number, and get the number if it is
(define (checkAndGetNum [v : Value]) : Number
  (type-case Value v
    [(NumV n) n]
    [else
     (error 'curlyTypeError
            (string-append "Expected Number, got function:"
                           (to-string v)))]))

;; Check if a value is a function, and get its parameter and body if it is
(define (checkAndGetFun [v : Value]) : (Symbol * Expr)
  (type-case Value v
    [(FunV x body)
     (pair x body)]
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

;; Convert a Value back into an equivalent expression
(define (value->expr [v : Value]) : Expr
  (type-case Value v
    [(NumV v) (NumLit v)]
    [(FunV x body) (Fun x body)]))


;; Evaluate Expressions
(define (interp [e : Expr] ) : Value
  (begin
;;     (display (to-string e))
;;     (display "\n------------------------------\n")
   (type-case Expr e
    ;; A number evaluates to itself
    [(NumLit n) (NumV n)]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    [(Plus l r)
     (liftVal2 + (interp l) (interp r))]
    ;; Works the same but for times
    [(Times l r)
     (liftVal2 * (interp l) (interp r))]
    ;; {if0 test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(If0 test thn els)
     (if (= 0 (checkAndGetNum (interp test)))
         (interp thn)
         (interp els))]
    ;; If we hit a variable in interp, it's an error
    [(Var x) (error 'interp (string-append "undefined variable " (to-string x)))]
    ;; NEW: Function calls
    ;; Have to evaluate the function now too
    [(Call funExpr argExpr)
     (let* ([argVal (interp argExpr)] ;;Evaluate the argument
            [funVal (checkAndGetFun (interp funExpr))] ;; Function might be an expression, so have to evaluate ;;NEW
            [argVar (fst funVal)]  ;; name of the function param
            [funBody (snd funVal)]) ;; body of the function
       ;; Replace the parameter with the argument value in the body,
       ;; then interpret the resulting body.
       ;; Need to substitute a NumLit because substition works on expressions,
       ;; not values.
       (interp (subst argVar (value->expr argVal) funBody)))]
    ;; interpret a let by replacing the bound variable
    ;; by its defined value in the body
    [(LetVar x xexp body)
     (interp
      (subst x
             (value->expr (interp xexp))
             body))]
    [(Fun x body)
     (FunV x body)])))

;; The Language Pipeline
;; We run  program by parsing an s-expression into a surface expression,
;; elaboarting (desugaring) the surface expression into a core expression,
;; then interpreting it
;;NEW: Running is always relative to a set of function definitions
;; so we take an extra argument that's a list of function definitions.
;;
(define (runWithDefs defs s-exp) (interp (elab (parse s-exp))))

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

(define (run s-exp) (interp (elab (parse (withFunctions s-exp)))))

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

;;NEW
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



