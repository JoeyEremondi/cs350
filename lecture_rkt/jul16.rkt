#lang plait

(define (foo [x : (Number -> Number)]) : Number
  ....)

(define bar : ((Number -> Number) * Boolean)
  (pair add1 #f))

(define (mapNum [f : (Number -> Number)]
                [xs : (Listof Number)]) : (Listof Number)
  (type-case (Listof Number) xs
             [empty empty]
             [(cons x rest)
                (cons (f x) (mapNum f rest))]))


(define (makeAdderWith n) : (Number -> Number)
  (lambda (x) (+ n x)))
(makeAdderWith 3)
(mapNum (makeAdderWith 3) '(1 2 3 4))


(define (+fun [f : (Number -> Number)]
              [g : (Number -> Number)]) : (Number -> Number)
  (lambda (x) (+ (f x) (g x))))

