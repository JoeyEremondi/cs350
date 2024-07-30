#lang plait

(define (length [xs : (Listof 'a)])
  (type-case (Listof 'a) xs
    [empty 0]
    [(cons h t) (+ 1 (length t))]))

(define (fast-length-helper [xs : (Listof 'a)]
                            [accum : Number])
  (type-case (Listof 'a) xs
    [empty accum]
    [(cons h t)
     (fast-length-helper t (+ 1 accum))]))

(define (fast-length xs)
  (fast-length-helper xs 0))

;;(define big-list (build-list 10000000 (lambda (x) x)))

(define (slow-factorial [n : Number]) : Number
  (if (<= n 0)
      1
      (* n (slow-factorial (- n 1)))))

;; Faster genrec way
(define (factorial-helper [n : Number]
                          [accum : Number]) : Number
  (if (<= n 0)
      accum
      (factorial-helper (- n 1) (* n accum))))

(define (fast-factorial n)
  (factorial-helper n 1))