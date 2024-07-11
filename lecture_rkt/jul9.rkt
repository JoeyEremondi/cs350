#lang plait

;; Surface syntax
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
  (SurfVar [x : Symbol]))

;; Core syntax
(define-type Expr
  (NumLit [n : Number])
  (Plus [left : Expr]
        [right : Expr])
  (Times [left : Expr]
         [right : Expr])
  (If0 [test : Expr]
       [thenCase : Expr]
       [elseCase : Expr]))

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
    [else (error 'parse (string-append "invalid input: " (to-string s)))]))

(define-type FunDef
  (mkFunDef [name : Symbol]
            [arg : Symbol]
            [body : Expr]))

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


(define (elab [expr : SurfaceExpr]) : Expr
  (type-case SurfaceExpr expr
    [(SurfNumLit n) (NumLit n)]
    [(SurfPlus l r) (Plus (elab l) (elab r))]
    [(SurfTimes l r) (Times (elab l) (elab r))]
    [(SurfIf0 test thn els) (If0 (elab test) (elab thn) (elab els))]
    [(SurfSub l r) (Plus (elab l) (Times (elab r) (NumLit -1)))]))

(define (interp [e : Expr] ) : Number
  (type-case Expr e
             [(NumLit n) n]
             [(Plus l r)
                (+ (interp l) (interp r))]
             [(Times l r)
                (* (interp l) (interp r))]
    [(If0 test thenCase elseCase)
     (let ([testVal (interp test)])
       (if (= 0 testVal)
           (interp thenCase)
           (interp elseCase)))]))