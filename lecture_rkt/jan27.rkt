#lang flit

(define (applyNTimes [f : (Number -> Number)]
                     [x : Number]
                     [nTimes : Number]) : Number

  (if (zero? nTimes)
      x
      (let* ([n-1 (- nTimes 1)]
             [rec (applyNTimes f x n-1)])
        (f rec))))

(define (timesTen x)
  (* 10 x))

(applyNTimes add1 3 10)

(applyNTimes timesTen 3 10)

(applyNTimes (lambda (x) (+ 3 (* 10 x))) 3 10)

(define (mapNum [f : (Number -> Number)]
                [xs : (Listof Number)]) : (Listof Number)
  (type-case (Listof Number) xs
             [empty empty]
             [(cons x rest)
                (cons (f x) (mapNum f rest))]))


(define (addNToEach [numToAdd : Number]
                    [xs : (Listof Number)]) : (Listof Number)
  (mapNum (lambda (x) (+ numToAdd x)) xs))

(addNToEach 3 '(1 2 3 4))

