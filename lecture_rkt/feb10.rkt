#lang flit

(define (list-length-slow (xs : [Listof 'a]))
  : Number
  (type-case (Listof 'a) xs
    [(empty) 0]
    [(cons someHead someTail)
       (+ 1 (list-length-slow someTail))]))

;; Helper function for tail recursive version

(define (length-helper [xs : (Listof 'a)]
                       [accum : Number])
  : Number
  (type-case (Listof 'a) xs
    [empty
       accum]
    [(cons someHead someTail)
       (length-helper someTail (+ 1 accum) )]))

(define (length xs) (length-helper xs 0))


(define (fast-fact-helper [n : Number]
                   [accum : Number])
  : Number
  (if (zero? n)
      accum
      (fast-fact-helper (- n 1) (* n accum))))

(define (fast-fact n) (fast-fact-helper n 1))