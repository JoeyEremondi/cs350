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