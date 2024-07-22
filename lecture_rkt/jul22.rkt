#lang plait

{letvar double {fun {x} {* x 2}}
  {letvar quadruple {fun {y} {double {double y}}}
    {letvar double 2
      {quadruple 3}}}}

;; Interpreter sees (LetVar 'double (Fun x ....))
;; Interprets to closure (ClosureV 'x (Times (NumLit 2) (Var x) emptyEnv)

;;Env is env1:
;; double := (ClosureV 'x (Times (NumLit 2) (Var x) emptyEnv)
;; evaluates (Fun 'y ....) to (ClosureV 'y .... (double := (ClosureV .... ....) )
;; i.e. it captures the environment
;; interp body in environment with quadruple added in

;; Now to interpret the 3rd let
;; have env
;; quadruple := (ClosureV ....)
;; double := (ClosureV ....)
;; let evaluates (NumLit 2) to 2, then interprets body in extended environment

;; Finally interpret {quadruple 3}
;; env is now
;;    double := 2
;;    quadruple := (ClosureV ....)
;;    double := (ClosureV .....) -- shadowed in this environment

;; to interpret {quadruple 3}
;; Evaluate the argument (NumLit 3) to 3
;; Evaluate function (Var 'quadruple)
;;   which gives (ClosureV 'y (Call (Var 'double) (Call (Var 'double) (Var 'y)) (double := (Clo... )

;; Now we interpret the body of the closure
;; but we use the environment packaged with the closure
;; So it interprets calls to double in environment where (double := (ClosureV ....)
;; So it does the multiplication and everything is good.
