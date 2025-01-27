#lang flit


(define (factorial [n : Number])
   : Number
  (if (= n 0)
      1 ;; zero case
      (let* ;;non-zero case
          ([n-1 (- n 1)]
           [factn-1 (factorial n-1)])
        (* n factn-1))))

(test (factorial 5) 120)
(test (factorial 0) 1)
(test (factorial 3) 6)

(define (sum [nums : (Listof Number)])
  : Number
  (if (empty? nums)
      0
      (let* ([h (first nums)]
             [t (rest nums)]
             [sumOfTail (sum t)])
        (+ h sumOfTail))))


(define (sumMatch [nums : (Listof Number)])
  : Number
  (type-case (Listof Number) nums ;; list we're matching
    [(empty)
     0] ;; result if empty 
    [(cons firstElem otherElems)
     (+ firstElem (sumMatch otherElems))])) ;;result if not

(test (sumMatch '()) 0)

(test (sumMatch (list 1 2 3)) 6)
(test (sumMatch (list 2 2 2 2)) 8)

(define (headOr0 [nums : (Listof Number)]) : Number
  (type-case (Listof Number) nums
    [(empty) 0]
    [(cons h t)
       (type-case (Listof Number) t
         [empty TODO]
         [(cons h2 t2) TODO])])
  )

;; Return everything but the head of the list
;; or, if it's empty, return an empty list
(define (safeRest [nums : (Listof Number)])
  : (Listof Number)
  (type-case (Listof Number) nums
    [(empty)
       '()]
    [(cons x y)
       y]))

