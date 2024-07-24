#lang slideshow
(require slideshow/code
         "utils/utils.rkt"
         "utils/arith.rkt")

(fullscreen-para-width!)

(define step-arrow
  (let ([p (t "→")])
    (lbl-superimpose p (inset p (/ (pict-width p) 3) 0 0 0))))

(define (questionable p)
  (refocus (hc-append
            (* 2 gap-size)
            p
            (scale (colorize (bt "?") "blue") 2))
           p))

;; ----------------------------------------

(slide (titlet "Part 1"))

(define double-example
  (code {define {double x}
          {+ x x}}
        code:blank
        {define {quadruple x}
          {double {double x}}}
        code:blank
        {quadruple 2}))

(slide
 #:title "Functions"
 'alts
 (list
  (list
   double-example
   'next
   (code #,step-arrow 8))
  (list
   (questionable
    (code {+ {define {double x} {+ x x}}
             1}))
   'next
   (blank)
   (blank)
   (para #:fill? #f "No: a function" (bit "definition") "is not an expression"))
  (list
   (questionable
    (code {+ {double 4}
             1}))
   'next
   (blank)
   (blank)
   (para #:fill? #f "Yes: a function" (bit "call") "is an expression")
   (blank)
   (blank)
   (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
     (para #:fill? #f
           "We'll use" (bit "call") "and" (bit "application") "interchangably")))))


(slide
 #:title "Function Definitions"
 (code {define {triple x}
         {+ x {+ x x}}})
 'next
 (blank)
 (vl-append
  (current-line-sep)
  (para #:fill? #f "A function has")
  (item #:fill? #f "a name")
  (item #:fill? #f  "an argument name")
  (item #:fill? #f "a" (it "body")))
 'next
 (blank)
 'alts
 (list
  (list
   (questionable
    (code
     (define-type Body-Exp
       (numE [n : Number])
       (idE [s : Symbol])
       (plusE [l : Body-Exp] [r : Body-Exp])
       (multE [l : Body-Exp] [r : Body-Exp])))))
  (list
   (para "Allow" (code x) "to be an expression, and then")
   (code {+ x {+ x x}})
   (para "is also an expression"))))

(define datatypes
  (code
   (define-type Exp
     (numE [n : Number])
     (idE [s : Symbol])
     (plusE [l : Exp] 
            [r : Exp])
     (multE [l : Exp]
            [r : Exp])
     (appE [s : Symbol]
           [arg : Exp]))
   code:blank
   (define-type Func-Defn
     (fd [name : Symbol] 
         [arg : Symbol] 
         [body : Exp]))))

(slide
 #:title "Functions and Function Calls"
 'alts
 (list
  (append (list*
           (car (get-expression-inventory))
           (item "identifiers")
           (cdr (get-expression-inventory)))
          (list
           (vc-append
            (current-line-sep)
            (item "function-call expressions")
            (subitem "a function name and an argument expression"))
           (colorize (hline (/ client-w 2) 0) "forestgreen")
           (vc-append
            (current-line-sep)
            (item "a function definition")
            (subitem "a function name, argument name, and body expression"))))
  (list
   datatypes)))

(define (rightward p)
  (inset p (* client-w 1/3) 0 0 0))

(define (attach-datatype p)
  (let ([p (rightward p)])
    (refocus (lt-superimpose
              (ct-superimpose p
                              (blank client-w 0))
              (frame (inset (scale datatypes (if widescreen? 1.0 0.7))
                            (/ gap-size 2))))
             p)))

(slide
 #:title "Representing Programs"
 'alts
 (list
  (list (attach-datatype (code {+ 1 2}))
        'next
        (blank)
        (blank)
        (rightward (code (plusE (numE 1) 
                                (numE 2)))))
  (list (attach-datatype (code {+ x 2}))
        'next
        (blank)
        (blank)
        (rightward (code (plusE (idE 'x) 
                                (numE 2)))))
  (list (attach-datatype
         (code {define {plus-two x}
                 {+ x 2}}))
        'next
        (blank)
        (blank)
        (rightward
         (code (fd 'plus-two
                   'x
                   (plusE (idE 'x)
                          (numE 2))))))
  (list (attach-datatype 
         (code {plus-two 9}))
        'next
        (blank)
        (blank)
        (rightward
         (code (appE 'plus-two 
                     (numE 9)))))
  (list (attach-datatype
         double-example)
        'next
        (blank)
        (rightward
         (scale
          (code 
           (list (fd 'double 'x 
                     (plusE (idE 'x)
                            (idE 'x)))
                 (fd 'quadruple 'x
                     (appE 'double 
                           (appE 'double
                                 (idE 'x)))))
           code:blank
           (appE 'quadruple (numE 2)))
          0.9)))))

;; ----------------------------------------

(slide (titlet "Part 2"))

(slide
 #:title "Evaluating Function Calls"
 (para
  (code
   {define {double x}
     {+ x x}}
   code:blank
   {double 3}))
 'next
 (para step-arrow (code {+ 3 3}))
 (para step-arrow (code 6))
 'next
 (blank)
 (blank)
 (code interp : (Exp (Listof Func-Defn) -> Number)
       code:blank
       get-fundef : (Symbol (Listof Func-Defn) -> Func-Defn)
       code:blank
       subst : (Exp Symbol Exp -> Exp)))
