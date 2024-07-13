#lang plait

;; EVERYTHING is the same except
;; for our new Environment definitions
;; and the definition of interp

;; Import all the curly-fundef functions,
;; except we'll rename interp
(require (rename-in "Curly-Fundef.rkt"
                    [interp slow-interp]
                    [run slow-run]
                    [subst old-subst]))

;;NEW: Pairs of variable and values
(define-type Binding
  (bind [name : Symbol]
        [val : Number]))

(test (bind-name (bind 'x 3)) 'x)
(test (bind-val (bind 'x 3)) 3)

;;NEW
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
(test (extendEnv (bind 'x 3) (extendEnv (bind 'y 4) empty))
      (list (bind 'x 3) (bind 'y 4)))

;;NEW
;; Linear search through environments to find a variable
;; Returns error if var not found
(define (lookup [n : Symbol] [env : Env]) : Number
  (type-case (Listof Binding) env
   ;; Can't find a variable in an empty env
   [empty (error 'lookup "undefined variable")]
   ;; Cons: check if the first binding is the var
   ;; we're looking for.
   ;; Return its value  if it is, otherwise
   ;; keep looking in the rest of the list
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))


;; Evaluate Expressions
;; NEW: relative to a given context
(define (interp [env : Env]
                [defs : (Listof FunDef)]
                [e : Expr] ) : Number
  (type-case Expr e
    ;; A number evaluates to itself
    [(NumLit n) n]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    [(Plus l r)
     (+ (interp env defs l) (interp env defs r))]
    ;; Works the same but for times
    [(Times l r)
     (* (interp env defs l) (interp env defs r))]
    ;; {if0 test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(If0 test thn els)
     (if (= 0 (interp env defs test))
         (interp env defs thn)
         (interp env defs els))]
    ;; NEW: We interpret variables by looking them up in the environment
    [(Var x)
      (lookup x env)]
    ;; NEW: Instead of substituting, we interpret in an extended environment
    [(Call funName argExpr)
     (let* ([argVal (interp env defs argExpr)] ;;Evaluate the argument
            [def (get-fundef funName defs)] ;; Look up the function definition
            [argVar (mkFunDef-arg def)] ;; name of the function param
            [funBody (mkFunDef-body def)]) ;; body of the function
       ;; NEW: Finish evaluating the function by interpreting
       ;; the function body in the environment with just the variable
       ;; Note: this is Static Scoping, because the only variable
       ;; the function can refer to is its argument
       ;; e.g. no global variables.
       ;; We need this for our functions to consistently evaluate to the same thing.
       (interp (extendEnv (bind argVar argVal) emptyEnv) ;; !!
               defs funBody))]))

;;NEW: example of what dynamic scoping looks like
;; Dynamic scoping is wrong and you shouldn't use it,
;; but you should understand it
(define (interp-dynscope [env : Env]
                [defs : (Listof FunDef)]
                [e : Expr] ) : Number
  (type-case Expr e
    [(NumLit n) n]
    [(Plus l r)
     (+ (interp-dynscope env defs l) (interp-dynscope env defs r))]
    [(Times l r)
     (* (interp-dynscope env defs l) (interp-dynscope env defs r))]
    [(If0 test thn els)
     (if (= 0 (interp-dynscope env defs test))
         (interp-dynscope env defs thn)
         (interp-dynscope env defs els))]
    [(Var x)
      (lookup x env)]
    [(Call funName argExpr)
     (let* ([argVal (interp-dynscope env defs argExpr)] ;;Evaluate the argument
            [def (get-fundef funName defs)] ;; Look up the function definition
            [argVar (mkFunDef-arg def)] ;; name of the function param
            [funBody (mkFunDef-body def)]) ;; body of the function
       ;; NEW: dynamic scoping extends the current environment instead of
       ;; starting with the empty environment for function calls
       (interp-dynscope (extendEnv (bind argVar argVal) env) ;; !!
               defs funBody))]))

;; we can use while testing
(define dynDefs
  (list
   (parse-fundef `{define {f x} {+ x y}})
   (parse-fundef `{define {g y} {f y}})
   ))

;; Show the difference between static and dynamic scoping
(test/exn (interp emptyEnv dynDefs  (elab (parse `{g 3})))
          "undefined")
(test (interp-dynscope emptyEnv dynDefs (elab (parse `{g 3})))
                 6)


;; The Language Pipeline
;; We run  program by parsing an s-expression into a surface expression,
;; elaboarting (desugaring) the surface expression into a core expression,
;; then interpreting it
;;NEW:
;; We always start running a program in an empty environment, since no variables
;; have been defined
(define (runWithDefs defs s-exp) (interp emptyEnv defs (elab (parse s-exp))))

(define (run s-exp) (interp emptyEnv testDefs (elab (parse s-exp))))

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
