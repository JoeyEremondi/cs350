#lang plait

(define (foldMap [f : ('a -> 'b)]
                 [xs : (Listof 'a)])
  : (Listof 'b)
  (foldr (lambda (hd mappedRestOfList) (cons (f hd) mappedRestOfList) )
         '()
         xs))

(define (sumList [xs : (Listof Number)]) : Number
  (foldl + 0 xs))

(define (sumListR [xs : (Listof Number)]) : Number
  (foldr + 0 xs))