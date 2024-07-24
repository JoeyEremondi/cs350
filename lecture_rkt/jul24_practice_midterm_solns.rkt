#lang plait

;; Q1

;; Dynamic vs static scope:
;; How to handle free variables in the body of a fn
;; Static: get them from environment
;;    at place of definition
;; Dynamic: get them from env
  ;; at place of call

{define {f x} {+ x 1}}
{f 4}
5 ;; for both

{define {f x} {+ x y}}
{f 4}
;; y not in scope
;; at define site or call site
;; so ERROR for both

{define {f x} {+ x y}}
{define {g y} {f y}}
{f 4}
;; y is only in scope for body of g.
;; y not in scope
;; at define site or call site
;; so ERROR for both

{define {f x} {+ x y}}
{define {g y} {f y}}
{g 4}
;; Dynamic scope:
;; evaluate body of g with y:=4
;; then
;; evaluate body of f.
;; Because dynamic scope, get free variables
;; in body of f from call site.
;; So we evaluate f body
;; in env (x := 4) (y := 4).
;; Result 8

;; Static scope:
;; evaluate body of g with y:=4
;; then
;; evaluate body of f.
;; Static scope gets free vars in f
;; their values from the define site.
;; No y in scope when f was defined.
;; so ERROR.

;; Q2
;; Answer: B) checking if given right number of args.
;; Why not A) ? In Curly, quote takes
;; string to s-expression.
;; So when reach parse, have already
;; read the input string for spaces
;; and parentheses.

;; Q3:
;; d) all of the above
;; Need to add to the parser so that
;; it can read the new syntax.
;; No desugaring, so the only way to
;; handle the new feature is to add
;; a variant in our AST i.e. Expr.
;; Finally, interp needs a case for
;; every expr variant, because it pattern
;; matches.

;; Q4:
#|
;; computes base^power
;; know that it's base * base * ... base
;; power times.
;; Recrcursion gives us a a magic way
;; to compute the right answer for n-1.
(define (exp [base : Number]
             [pow : Number) : Number
(if (= power 0)
  1
  (* base (exp base (- power 1)))
))

;; Now our interp case is
[(Pow base power)
  (exp (interp base) (interp power))]

;; other way to do it is without helper function
[(Pow base power)
   (let* ([baseV (interp base)]
           [powV (interp pow)])
    (if (= powV 0)
      1
    (* baseV
       (interp (Pow (NumLit baseV)
                    (NumLit (- powV 1)) )))
    )
   ]
|#

;; Q5
#|
[(SurfLetVar* bindings body)
  (type-case (Listof (Symbol * SurfaceExpr) bindings
    [empty
       (elab body)]
    [(cons bnd bndRest)
       (LetVar (fst bnd)
               (elab (snd bnd))
               (elab (SurfLetVar* bndRest body)))
     ]
  )
  ]
|#

#|
(define (*fun [f : (Number -> Number)]
                     [g : (Number -> Number)]) : (Number -> Number)
;; Your implementation goes here
  (lambda ([x : Number]) (* (f x) (g x)) )
)
|#

;; Q7
;; missing problem description
;; should say
;; "produces a (Listof 'c)"
;; that's f applied to each
;; pair of the list

;; a)
;; ((('a * 'b) -> 'c) (Listof ('a * 'b)) -> (Listof 'c))

;;
#|
(define (mapPair f pairs)
;; your implementation goes here
  (type-case pairs
     [empty empty]
     [(cons pr rest)
       (cons (f pr) (mapPair f rest) ])


)

|#