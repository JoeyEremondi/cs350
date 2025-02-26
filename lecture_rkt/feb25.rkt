#lang flit

;; fib(0) = 0
;; fib(1) = 1
;; fib(n) = fib(n-1) + fib(n-2)

;; (define (fib n)
;;   (foldl
;;    (lambda (nextElem prevElems)))
;;   )

(define (fold-reverse xs)
  (foldl (lambda (el accum) (cons el accum))
         empty
         xs))

(define (foldr-reverse xs)
  (foldr (lambda (el accum) (cons el accum))
         empty
         xs))




(define (sum-list-l xs)
  (foldl + 0 xs))

(define (sum-list-r xs)
  (foldr + 0 xs))

(define (map f xs)
  (type-case (Listof 'a) xs
    [(empty) empty]
    [(cons h t)
       (let ((tailMap (map f t)))
       (cons (f h) tailMap))]))

;; 
;; (lambda (h tailMap)
;;   (cons (f h) tailMap))

(define (f x) (+ x 3))
