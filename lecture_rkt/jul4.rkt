#lang plait

(define (sumEveryOtherElement [xs : (Listof Number)])
  : Number
  (cond
    [(empty? xs) 0]
    [(empty? (rest xs)) 0]
    [else (+ (first (rest xs)) (sumEveryOtherElement (rest (rest xs))))])
;;   (if (empty? xs)
;;       0
;;       (if (empty? (rest xs))
;;           0
;;           (+ (first (rest xs)) (sumEveryOtherElement (rest (rest xs)))) ))
  )

(define (concatBad [xs : (Listof 'elem)]
                [ys : (Listof 'elem)])
  : (Listof 'elem)
  (type-case (Listof 'elem) ys
    [empty
     xs]
    [(cons h t)
     (cons h (concatBad xs t))]))

(define (flatten (xs : (Listof (Listof Number)))) : (Listof Number)
  (type-case (Listof (Listof Number)) xs
    [empty empty]
    [(cons firstList otherLists)
     (let ([flatOthers (flatten otherLists)])
       (append firstList flatOthers))
       ])
  )

(define (sort (xs : (Listof Number))) : (Listof Number)
  (type-case (Listof Number) xs
  [empty empty]
  [(cons h t)
   (let* ([smaller (filter (lambda (x) (<= x h)) t)]
          [greater (filter (lambda (x) (> x h)) t)] )
          (append (sort smaller) (cons h (sort greater))))]))

(test (sort '(5 4 3 2 99 1 -1000000)) '(-1000000 1 2 3 4 5 99))

(define-type Infinite
  (Loop (x : Infinite)))

(test (flatten '((1 2 3) (4 5 6) (7 8 9))) '(1 2 3 4 5 6 7 8 9))

(test (concatBad '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))


(test (sumEveryOtherElement (list 1 2 3 4 5 6 7)) 12)