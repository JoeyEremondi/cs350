#lang plait

;; Curly-Sub: A programming language with syntactic sugar
;; Exactly like Curly-Cond, but we add subtraction
;; Note: Expr and Interp do not change *at all*.
;; But we add a new datatype for surface syntax
;; and an extra translation before interpretation

;; BNF for Curly-Sub
;; New form: {- e1 e2}
;; Evaluates to the difference between e1 and e2
;; 
;;  <expr> ::=
;;     "{" "+" <expr> <expr> "}"
;;   | "{" "*" <expr> <expr> "}"
;;   | "{" "if0" <expr> <expr> <expr> "}"
;;   | "{" "-" <expr> <expr> "}"
;;   | NUMBER

;; NEW: Surface AST
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
           [right : SurfaceExpr]))

;; Parse
;; Takes an S-expression and turns it into an SurfExpr
;; Raises an error if it doesn't represent a valid program
;; NEW: parse now produces a surface expression
;; and has a case for subraction
(define (parse [s : S-Exp]) : SurfaceExpr
  (cond
    ;; Constant number e.g. 5
    [(s-exp-match? `NUMBER s) (SurfNumLit (s-exp->number s))]
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
    [else (error 'parse "invalid input")]))


;; Abstract syntax for Curly-Sub
;; NOT NEW At all
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
       [elseCase : Expr]))

;; NEW:
;; Elaborate (desugar) surface expressions into
;; core expressions
;; For every case except for Sub, there's a core constructor
;; that takes the exact same arguments as the surface constructor
;; (except that its sub-expressions are core, not surface)
;; so we can just apply the corresponding Core constructor and apply elab recursively
(define (elab [expr : SurfaceExpr]) : Expr
  (type-case SurfaceExpr expr
    ;; Trivial cases: expressions that are the same in the surface and core syntax
    [(SurfNumLit n)
     (NumLit n)]
    ;; For plus, times, and if, we need to recursively elaborate the sub-expressions
    ;; because they might contain a subtraction
    [(SurfPlus l r)
     (Plus (elab l) (elab r))]
    [(SurfTimes l r)
     (Times (elab l) (elab r))]
    [(SurfIf0 test thn els)
     (If0 (elab test) (elab thn) (elab els))]
    ;; NEW: The interesting case for elaboration
    ;; {- e1 e2} becomes {+ e1 {* -1 e2}}
    [(SurfSub l r) (
                    Plus (elab l) (Times (elab r) (NumLit -1)))]))


;; Evaluate Expressions
;; NOT NEW at all
(define (interp [e : Expr] ) : Number
  (type-case Expr e
    ;; A number evaluates to itself
    [(NumLit n) n]
    ;; {+ e1 e2} evaluates e1 and e2, then adds the results together
    [(Plus l r)
     (+ (interp l) (interp r))]
    ;; Works the same but for times
    [(Times l r)
     (* (interp l) (interp r))]
    ;; {if0 test thn els} evaluates test and checks if it's zero
    ;; if it is, then we evaluate thn
    ;; otherwise we evaluate els
    [(If0 test thn els)
     (if (= 0 (interp test))
         (interp thn)
         (interp els))]))

;; The Language Pipeline
;; We run  program by parsing an s-expression into a surface expression,
;; elaboarting (desugaring) the surface expression into a core expression,
;; then interpreting it
;; NEW: We now have an elaboration step
;; Implicit: we can turn strings into s-expressions using Racket's quote
;; i.e. `{+ 3 4} generates an S-expression directly
(define (run s-exp) (interp (elab (parse s-exp))))

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
