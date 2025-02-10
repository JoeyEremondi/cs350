#lang flit


(define (sortBy [compare : ('a 'a -> Boolean)]
                [xs : (Listof 'a)]) : (Listof 'a)
  (type-case (Listof 'a) xs
    [empty
     empty]
    [(cons first rest)
     (let*
         ([smallers
           (filter (lambda (x) (compare x first))
                   rest)]
          [biggers
           (filter (lambda (x) (not (compare x first)))
                   rest)])
       (append (sortBy compare smallers)
               (cons first
                     (sortBy compare biggers))))]))


(define (notF (f : ('a -> Boolean)))
  : ('a -> Boolean)
  (lambda (x) (not (f x))))

(define (o [g : ('b -> 'c)]
           [f : ('a -> 'b)])
  : ('a -> 'c)
  ;; Goal is ('a -> 'c)
  ;; have f and g
  (lambda (x)
    ;; x in scope of type a
    ;; want a value of type c
    (let ([fx (f x)]) ;; type b
      (g fx)) ;; g produces type 'c
    )
  )

(define (curry [f : ('a 'b -> 'c)]
               [x : 'a])
  : ('b -> 'c)
  (lambda (y) (f x y)))

(define (flip f) (lambda (x y) (f y x)))

(define otherEven
  (o (curry = 0) (curry (flip modulo) 2)))