#lang plait

(filter (lambda (x) (> x 1000)) '(1 2 3 4))

(map (lambda (x) (> x 1000)) '(1 2 3 4))

(define (compare [x : 'a] [y : 'a]) : Boolean
  ....)




(define (sortBy [compare : ('elemType 'elemType -> Boolean)]
                [xs : (Listof 'elemType)]) : (Listof 'elemType)
  (type-case (Listof 'elemType) xs
    [empty
     empty]
    [(cons first rest)
     (let*
         (
          [smallers
           (filter (lambda ([x : 'elemType])
                     (compare x first))
                   rest)]
          [biggers
           (filter (lambda (x) (not (compare x first)))
                   rest)])
       (append (sortBy compare smallers)
               (cons first
                     (sortBy compare biggers))))]))

(lambda (x) (> x 1000))

(map some (list "hello" "goodbye"))



;(map (curry 2 (flip modulo))
;     '(1 2 3 4 5 6 7 8))
