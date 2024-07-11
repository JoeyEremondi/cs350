#lang plait

;; Parser for function definitions
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



(define-type Expr
  (NumLit [n : Number])
  (Plus [left : Expr]
        [right : Expr])
  (Times [left : Expr]
         [right : Expr])
  (If0 [test : Expr]
       [thenCase : Expr]
       [elseCase : Expr])
  (Var [x : Symbol])
  (Call [funName : Symbol]
        [funArg : Expr]))

(define-type FunDef
  (mkFunDef [name : Symbol]
            [arg : Symbol]
            [body : Expr]))



(define (parse [s : S-Exp]) : SurfaceExpr
  (cond
    [(s-exp-match? `NUMBER s) (SurfNumLit (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (SurfVar (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (SurfPlus (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (SurfTimes (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{- ANY ANY} s)
     (SurfSub (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (SurfIf0 (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    ;; Has to come last, only treat as a function call if it's not a binary operator
    [(s-exp-match? `{SYMBOL ANY} s)
     (SurfCall (s-exp->symbol (first (s-exp->list s)))
            (parse (second (s-exp->list s))))]
    [else (error 'parse (string-append "invalid input: " (to-string s)))]))


(define-type SurfaceExpr
  (SurfNumLit [n : Number])
  (SurfPlus [left : SurfaceExpr]
        [right : SurfaceExpr])
  (SurfTimes [left : SurfaceExpr]
         [right : SurfaceExpr])
  (SurfIf0 [test : SurfaceExpr]
       [thenCase : SurfaceExpr]
       [elseCase : SurfaceExpr])
  (SurfSub [left : SurfaceExpr]
           [right : SurfaceExpr])
  (SurfVar [x : Symbol])
  (SurfCall [funName : Symbol]
            [funArg : SurfaceExpr]))


(define (elab [expr : SurfaceExpr]) : Expr
  (type-case SurfaceExpr expr
    [(SurfNumLit n) (NumLit n)]
    [(SurfPlus l r) (Plus (elab l) (elab r))]
    [(SurfTimes l r) (Times (elab l) (elab r))]
    [(SurfIf0 test thn els) (If0 (elab test) (elab thn) (elab els))]
    [(SurfSub l r) (Plus (elab l) (Times (elab r) (NumLit -1)))]
    [(SurfVar x) (Var x)]
    [(SurfCall funName arg) (Call funName (elab arg))]))


(define defs
  (list
   (parse-fundef `{define {add5 x} {+ x 5}})
   (parse-fundef `{define {double y} {* 2 y}})
   (parse-fundef `{define {quadruple x} {double {double x}}})
   (parse-fundef `{define {checkIf0 x} {if0 x 1 0}})))

(define (run [s-exp : S-Exp])
  (interp defs (elab (parse s-exp))))

(define (get-fundef [s : Symbol] [defs : (Listof FunDef)]) : FunDef
  (type-case (Listof FunDef) defs
    [empty (error 'get-fundef (string-append "No function with name " (to-string s)))]
    [(cons def otherDefs)
     (if (equal? (mkFunDef-name def) s)
         def
         (get-fundef s otherDefs))]))

(define (subst [toReplace : Symbol]
               [replacedBy : Expr]
               [replaceIn : Expr]) : Expr
  (type-case Expr replaceIn
    [(NumLit n) (NumLit n)]
    [(Var x)
     (if (equal? x toReplace)
         replacedBy
         (Var x))]
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
    ;; Have to decide how to handle namespaces
    [(Call funName arg)
     (Call funName (subst toReplace replacedBy arg))]))


(define (interp
           [defs : (Listof FunDef)]
           [e : Expr]) : Number
  (type-case Expr e
             [(NumLit n) n]
             [(Plus l r)
                (+ (interp defs l) (interp defs r))]
             [(Times l r)
                (* (interp defs l) (interp defs r))]
    [(If0 test thenCase elseCase)
     (let ([testVal (interp defs test)])
       (if (= 0 testVal)
           (interp defs thenCase)
           (interp defs elseCase)))]
    [(Var x) (error 'interp "undefined variable")]
    [(Call funName argExpr)
     (let* ([argVal (interp defs argExpr)]
           [def (get-fundef funName defs)]
           [argVar (mkFunDef-arg def)]
           [funBody (mkFunDef-body def)])
       (interp defs (subst argVar (NumLit argVal) funBody)))]))