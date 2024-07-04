#lang plait

;; Exponentiation
(define (exp [base : Number]
             [power : Number]) : Number
  (if (zero? power)
      1
      (let* ([power-1 (- power 1)]
            [recResult (exp base power-1)])
        (* base recResult))))


(test (exp 2 3) 8)
(test (exp 3 0) 1)
(test (exp 1000 1) 1000)

(define x 3)
(define y 4)


(define (increment [xs : (Listof Number)])
        : (Listof Number)
  (type-case (Listof Number) xs
    [empty
       empty]
    [(cons h t)
       (cons (+ h 1) (increment t))]))


(define-type Shape
  (Rectangle [length : Number]
             [width : Number])
  (Circle [radius : Number])
  (Line )
  (Line2)
  (SomethingElse))

(define tv (Rectangle 16 9))
(define loonie (Circle 1))

(define (area [shp : Shape]) : Number
  (type-case Shape shp
     [(Rectangle l w)
       (* l w)]
     [(Circle r)
       (* 3.14 (* r r))]
    [else 0])
)
(area tv)
(area loonie)


(define (factorial (n : Number))
  : Number
  (if (= n 0)
      1
      (let*
          ([n-1 (- n 1)]
           [fn-1 (factorial n-1)])
          (* n fn-1)) ;;(* n (factorial (- n 1)))
      ))
