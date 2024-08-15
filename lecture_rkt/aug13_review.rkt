#lang plait
(define-type Building
  (House [numOccupants : Number]
         [streetAddress : String])
  (Store [businessName : String]
         [streetAddress : String]))


(define (reverse l) (foldl cons empty l))



(define (curry [f : ('a 'b -> 'c)]
               [x : 'a])
  : ('b -> 'c)
  (lambda (y) (f x y)))

(map (curry + 5) '(1 2 3))
;f is +, x is 5 for curry

;;Replace curry with its body
;; with f replaced by +
;; and x replaced by 5

;(map (lambda (y) (f x y)) '(1 2 3))
(map (lambda (y) (+ 5 y)) '(1 2 3))

;; Same as writing
(map (lambda (y) (+ 5 y)) (cons 1 '(2 3)))

;(cons (f h) (map f t) where
; f is (lambda (y) (+ 5 y))
; h is 1
; t is '(2 3)

(cons ((lambda (y) (+ 5 y)) 1) (map (lambda (y) (+ 5 y)) '(2 3)))
;; Now we have a lambda applied to a value directly
;; So we evaluate it like a function call (which it is)
;; (cons (+ 5 y) (map ...)) where y := 1
(cons (+ 5 1) (map (lambda (y) (+ 5 y)) '(2 3)))

(cons 6 (map (lambda (y) (+ 5 y)) '(2 3)))


;;;;;;;;;;;;;;;;;;;; Tail recursion

;; assume power is non-negative whole number
(define (exp [base : Number]
             [pow : Number]) : Number
  (exp-helper base pow 1))

;; Multiply the accumulator by base, pow times
;; pow tells us how many times we have left to multiply
(define (exp-helper base pow accum)
  ;; check if we've run out of multiplications we need to do
  (if (= pow 0)
      ;;base case always returns accum
      accum
      ;; recursive case: call ourselves
      ;; with new value for counter (it's one less)
      ;; and for accum (do one more multiplication)
      (exp-helper base (- pow 1) (* accum base))))

;; Weighted average
;; take a list of Number*Number pairs
;; where the first is a number, and the second is the weight
;; that number should have in the average.
(define (weighted-avg [pairs : (Listof (Number * Number))]) : Number
  (weighted-avg-helper pairs 0))

;; helper
(define (weighted-avg-helper [pairs : (Listof (Number * Number))]
                             [sumSoFar : Number])
  (type-case (Listof (Number * Number)) pairs
    [empty sumSoFar]
    [(cons h t)
     ;; h : (Number * Number)
     (weighted-avg-helper t (+ sumSoFar (* (fst h) (snd h))))]))

(define (weighted-avg-fold [pairs : (Listof (Number * Number))])
  (foldl (lambda (h sumSoFar)
           (+ sumSoFar (* (fst h) (snd h))))
         0 pairs))