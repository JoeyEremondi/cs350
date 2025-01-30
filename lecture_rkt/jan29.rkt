#lang flit

(define (mapNum [f : (Number -> Number)]
                [xs : (Listof Number)]) : (Listof Number)
  (type-case (Listof Number) xs
    [empty empty]
    [(cons x rest)
     (cons (f x) (mapNum f rest))]))

(define (makeAdderWith [n : Number])
  : (Number -> Number)
  (lambda (x) (+ x n)))

(define add3 (makeAdderWith 3))

(add3 5)
(add3 -3)

(mapNum add3 '(1 2 3))

(mapNum (makeAdderWith 3) '(1 2 3))





(define (liftOption [f : (Number -> Number)])
  : ((Optionof Number) -> (Optionof Number))
  (lambda (optionX)
    (type-case (Optionof Number) optionX
      [(none)
       (none)]
      [(some x)
       (some (f x))])))



(define-type (Optionof 'a)
  (none)
  (some [x : 'a]))

(define (sortBy [xs : (Listof 'a)]
                [f : ('a 'a -> Boolean)]) TODO)




