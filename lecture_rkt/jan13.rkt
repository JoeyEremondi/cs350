#lang flit


(define (max [x : Number]
             [y : Number])
  : Number
  (if (< x y)
      y
      x))


(test (max 3 4) 4)
(test (max 1 0) 1)

(define (factorial [n : Number]) : Number
  (if (zero? n)
      1
      ;;We know n = m+1 for some m
      (let* ([n-1  (- n 1)]
             ;[recResult (factorial n-1)]
             )
        ;; n : Number
        ;;n-1 : Number
        ;; recResult : Number = (factorial n-1)
        n-1
      )))

(test (factorial 0) 1)
(test (factorial 1) 1)
(test (factorial 2) 2)
(test (factorial 3) 6)


































3