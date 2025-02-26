#lang flit

(define-type Exp
  (num [n : Number])
  (plus [l : Exp] [r : Exp])
  (times [l : Exp] [r : Exp])
  (cnd [test : Exp]
       [thenCase : Exp]
       [elseCase : Exp])
  )

(define (foo [e : Exp])
  (plus (num TODO) TODO))

(plus (num 3) (times (num 2) (num 4)))

(define-type-alias Value Number)

(define (interp [e : Exp])
  : Value
  (type-case Exp e
    [(num n)
       n]
    [(plus l r)
       (+ (interp l) (interp r))]
    [(times l r)
       (* (interp l) (interp r))]
    [(cnd test thn els)
       (if (let ([testVal (interp test)])
             (= testVal 0))
           (interp thn)
           (interp els))]))