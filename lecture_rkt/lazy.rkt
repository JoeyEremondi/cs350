#lang plait #:lazy
;;;;;;;;;;;;;;;;;; Use the extra flag to make plait lazy

;; Lazy evaluation: we don't evaluate arguments until we have to
;; For example, if we have an error in an expression that's never used,
;; it is never evaluated

(define fail : 'a
  (error 'fail "FAIL was evaluated"))

(define failB : Boolean
  (error 'fail "FAIL was evaluated"))

;; The argument x is never used, so the error is never evaluated
(define (ignore x) 3)

(test
   (ignore fail)
   3)

(define (if0Fst x y)
  (if (= x 0) y (* x 2)))

;; There are "strictness points" where we need to inspect
;; the value of an expression to proceed,
;; e.g. type-case and if, arithmetic, equality comparisons, etc.
;; So these can raise an error

;;e.g. 
(test/exn (zero? fail) "")


;; We can build infinite lists with lazy evaluation.
;; Map works just fine over an infinite list, since it doesn't
;; evaluate the tail until we try to access it.
(define allNumbers
  (cons 1 (map add1 allNumbers)))

(test (first allNumbers)
      1)

(test (second allNumbers)
      2)

(test (list-ref allNumbers 199362)
      199363)

(define (allNumbersAfter n)
  (map (lambda (x) (+ x n)) allNumbers))

;; Can get all evens by mapping *2 to all numbers
(define allEvens
  (map (lambda (x) (* x 2))
       allNumbers))

;; Then add 1 to get all odds.
(define allOdds (map add1 allEvens))

;; Alternately, we can filter out all the odd numbers
;; to get a list of all evens
(define allEvensFilter
  (filter (lambda (x) (= 0 (modulo x 2)))
          allNumbers))

;; We can take any finite prefix of an inifnite list
(define (take [n : Number]
         [l : (Listof 'a)]) : (Listof 'a)
  (if (<= n 0)
      '()
      (cons (first l)
            (take (- n 1) (rest l)))))


;; We can search by making an infinite (or very long) lists and filtering to
;; only include the ones that have the properties we want.
;; e.g. (very slow) way to find a solution to an equation

(define (isSolution x)
  (= (- (* x x) 625) 0))


(define (range start end step)
  (if (<= end start)
      '()
      (cons start
            (range (+ start step) end step))))


(filter isSolution (range 0 1000000 1))
