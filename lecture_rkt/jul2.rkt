#lang plait

;; Read Evaluate Print Loop

;; Type error
;(define (foo x) (+ 3 #f))


;;Figures out the types is Number -> Number
(define (add2 x) (+ x 2))

;;Figures out the type is generic
(define (singleton x) (list x))

(define (sayHello) (display "Hello\n"))
sayHello ;; value
(sayHello) ;;calls the function and prints

(define (helloWithArg x) (display "Hello\n"))

(define (+-*/?! x) x)
(+-*/?! 3)