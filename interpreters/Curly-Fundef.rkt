#lang plait


;; Curly-Fundef: A programming language with function definitions

;; BNF for Curly-Fundef
;; New forms: x (variable) and {e1 e2} (function calls)
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if0" <expr> <expr> <expr> "}"
;;   | "{" "-" <expr> <expr> "}"
;;   | SYMBOL
;;   | "{" SYMBOL <expr> "}"
;;   | NUMBER

;; NEW:
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
  ;; NEW: Variables (will show up in function definitions)
  (SurfVar [x : Symbol])
  ;; NEW: Function calls
  (SurfCall [funName : Symbol]
            [arg : SurfaceExpr]))

;; Parse
;; Takes an S-expression and turns it into an SurfExpr
;; Raises an error if it doesn't represent a valid program
;; NEW: parse now produces a surface expression
;; and has a case for subraction
(define (parse [s : S-Exp]) : SurfaceExpr
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (SurfNumLit (s-exp->number s))]
    ;; NEW: Variable is just a Racket symbol
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
    ;; NEW
    ;; Function calls {f e1}
    ;; Has to come last, only treat as a function call if it's not a binary operator
    [(s-exp-match? `{SYMBOL ANY} s)
     (SurfCall (s-exp->symbol (first (s-exp->list s)))
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
  ;; NEW: Variables (will show up in function definitions)
  (Var [x : Symbol])
  ;; NEW: Function calls
  (Call [funName : Symbol]
        [arg : Expr]))

;;NEW
;; Datatype for storing function definitions
(define-type FunDef
  (mkFunDef [name : Symbol] ;; name of the function we're calling
            [arg : Symbol] ;; name of the function's parameter
            [body : Expr] ;; expression to run when calling the function
            ))

;;NEW
;; Parser for function definitions.
;; Parses {define {f x} e} into (mkFunDef f x (elab (parse e))
(define (parse-fundef [s : S-Exp]) : FunDef
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL} ANY} s)
     (mkFunDef
        (s-exp->symbol
           (first (s-exp->list (second (s-exp->list s)))))
         (s-exp->symbol
            (second (s-exp->list (second (s-exp->list s)))))
         (elab (parse (third (s-exp->list s)))))]
    [else (error 'parse-fundef "invalid fundef input")]))


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
    ;; NEW: variables are core expressions, so there's nothing to do for desugaring
    [(SurfVar x) (Var x)]
    [(SurfCall f arg)
     (Call f (elab arg))]))


;; NEW:
;; Helper function to iterate through a list of function definitions
;; and find if there is one with the given name
(define (get-fundef [s : Symbol] [defs : (Listof FunDef)]) : FunDef
  (type-case (Listof FunDef) defs
    ;; If we hit empty list, then we didn't find the function
    [empty (error 'get-fundef (string-append "No function with name " (to-string s)))]
    ;; Check if the first one in the list is what we're looking for
    ;; If it is, return it.
    ;; Otherwise, look in the rest of the list.
    [(cons def otherDefs)
     (if (equal? (mkFunDef-name def) s)
         def
         (get-fundef s otherDefs))]))

;; NEW:
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
    ;; We can't replace a function name, since the Expr AST requires that the function name be
    ;; a symbol, not an expression. So we just recursively replace in the argument.
    [(Call funName arg)
     (Call funName (subst toReplace replacedBy arg))]))



;; Evaluate Expressions
;; NEW: relative to a given context
(define (interp [defs : (Listof FunDef)] ;;NEW
                [e : Expr] ) : Number
  (type-case Expr e
    ;; A number evaluates to itself
    [(NumLit n) n]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    [(Plus l r)
     (+ (interp defs l) (interp defs r))]
    ;; Works the same but for times
    [(Times l r)
     (* (interp defs l) (interp defs r))]
    ;; {if0 test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(If0 test thn els)
     (if (= 0 (interp defs test))
         (interp defs thn)
         (interp defs els))]
    ;; NEW: If we hit a variable in interp, it's an error
    [(Var x) (error 'interp "undefined variable")]
    ;; New: Function calls
    [(Call funName argExpr)
     (let* ([argVal (interp defs argExpr)] ;;Evaluate the argument
            [def (get-fundef funName defs)] ;; Look up the function definition
            [argVar (mkFunDef-arg def)] ;; name of the function param
            [funBody (mkFunDef-body def)]) ;; body of the function
       ;; Replace the parameter with the argument value in the body,
       ;; then interpret the resulting body.
       ;; Need to substitute a NumLit because substition works on expressions,
       ;; not values.
       (interp defs (subst argVar (NumLit argVal) funBody)))]))

;; The Language Pipeline
;; We run  program by parsing an s-expression into a surface expression,
;; elaboarting (desugaring) the surface expression into a core expression,
;; then interpreting it
;;NEW: Running is always relative to a set of function definitions
;; so we take an extra argument that's a list of function definitions.
;;
(define (runWithDefs defs s-exp) (interp defs (elab (parse s-exp))))

;;NEW: A list of helpful function definitions
;; we can use while testing
(define testDefs
  (list
   (parse-fundef `{define {add5 x} {+ x 5}})
   (parse-fundef `{define {double y} {* 2 y}})
   (parse-fundef `{define {quadruple x} {double {double x}}})
   (parse-fundef `{define {checkIf0 x} {if0 x 1 0}})
   (parse-fundef `{define {const0 x} {* 0 {+ 2 {+ 3 {if0 0 {- 3 999999} 40000}}}}})))

;;NEW:
;; For testing, we have run work by interpreting
;; in the context of the above definitions

(define (run s-exp) (interp testDefs (elab (parse s-exp))))

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

(test (run `{- 2 1})
      1)
(test (run `{* 2 {- 3 5}})
      -4)

(test (run `{if0 {- 2 2} {- 5 3} {- 3 5}})
      2)

(test (run `{if0 {- 2 4} {- 5 3} {- 3 5}})
      -2)


(test/exn (parse `{1 2}) "invalid input")


(test/exn (run `{+ x 3}) "undefined variable")

(test (run `{if0 0 3 {+ x 3}})
      3)

(test (run `{add5 10})
      15)

(test (run `{add5 {if0 0 3 4}})
      8)

(test (run `{if0 {add5 -5} 3 4})
 3)

(test (run `{quadruple 4})
      16)

(test (run `{checkIf0 {+ 3 -3}})
 1)

(test (run `{const0 3})
      0)

