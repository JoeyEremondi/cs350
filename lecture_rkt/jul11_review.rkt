#lang plait

;; In a Racket function,
;; the thing we return is the body of the function
(define (foo x)
  ;; Whatever goes here is the return of the function
  (+ x 1))

(+ 1 (foo 10))

(define (abs x)
  (if (< x 0)
      (* x -1)
      x))

(define-type (Optionof 'x)
  (some [val : 'x])
  (none))

(define (checkIfSome (opt : (Optionof 'x))) : String
  (if
   (type-case (Optionof 'x) opt
     ;;[(PatternToMatchAgainst fieldVariable fieldVar2 ...)=expressionToProduceIfMatch]
     [(some x)
      #t]
     [(none)
      #f])
   "trueString"
   "falseString")
  )

;;Always need brackets around pattern, even if 0 fields
;; EXCEPT for empty for lists

(define (increment [xs : (Listof Number)])
  : (Listof Number)
  (type-case (Listof Number) xs
    [empty
     empty]
    [(cons h t)
     (cons (+ h 1) (increment t))]))

(increment (cons 1 (cons 2 empty)))
; =
(type-case (Listof Number) (cons 1 (cons 2 empty))
  [empty
   empty]
  [(cons h t)
   (cons (+ h 1) (increment t))])
; =
(cons (+ 1 1) (increment (cons 2 empty)))

(cons 2
      (cons (+ 2 1) (increment '())))

(cons 2
      (cons (+ 2 1)
            (type-case (Listof Number) empty
              [empty
               empty]
              [(cons h t)
               (cons (+ h 1) (increment t))])))

(cons 2
      (cons 3
            empty))