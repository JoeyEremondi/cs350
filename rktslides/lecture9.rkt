#lang slideshow

(require (lib "code.ss" "slideshow")
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss")

(define (at* s)
  (colorize (t s) (current-id-color)))
(define (at s)
  (inset (at* s) (* 4 (current-line-sep))))

;; ----------------------------------------
(slide (titlet "Part 1"))

(define reduction
  (code (+ (* 4 3) (- 8 7)) #,sym:implies (+ 12 (- 8 7)) #,sym:implies (+ 12 1)))

(slide
 #:title "Plait vs. Algebra"
 (blank)
 'alts
 (list
  (list
   (para " ")
   reduction)
  (list
   (para #:width (get-client-w #:aspect 'fullscreen)
         "In Plait, we have a specific order for evaluating sub-expressions:")
   reduction))
 'next
 (blank)
 (blank)
 (para #:width (get-client-w #:aspect 'fullscreen)
       "In Algebra, order doesn't matter:")
 (para #:fill? #f (at "(4\u00B73)+(8-7)") sym:implies (at "12+(8-7)") sym:implies (at "12+1"))
 (bt "or")
 (para #:fill? #f (at "(4\u00B73)+(8-7)") sym:implies (at "(4\u00B73)+1") sym:implies (at "12+1")))

(slide
 #:title "Algebraic Shortcuts"
 (para "In Algebra, if we see")
 (vl-append (current-line-sep)
            (at "f(x, y) = x")
            (at "g(z) = ...")
            (at "f(17, g(g(g(g(g(18))))))"))
 (para "then we can go straight to")
 (at "17")
 (para "because the result of all the" (at* "g") "calls"
       "will not be used")
 'next
 (blank)
 (blank)
 (colorize (para #:fill? #f "But why would a programmer write something like that?")
           BlueColor))

(slide
 #:title "Avoiding Unnecessary Work"
 (scode (code:contract layout-text : String w h -> pict)
        (define (layout-text txt w h)
          (local [(define lines
                    (code:comment "lots of work to flow a paragraph")
                    ...)]
            
            (make-pict w
                       h
                       (lambda (dc x y)
                         (code:comment "draw paragraph lines")
                         ...))))
        ...
        (define speech (layout-text "Four score..."
                                    800
                                    600))
        ...
        (pict-width speech)))

(slide
 #:title "Avoiding Unnecessary Work"
 (scode (code:contract read-all-chars : file -> list-of-char)
        (define (read-all-chars f)
          (if (at-eof? f)
              empty
              (cons (read-char f) (read-all-chars f))))
        ...
        (define content (read-all-chars (open-file user-file)))
        (if (equal? (first content) #\#)
            (process-file (rest content))
            (error 'parser "not a valid file"))))

(slide
 #:title "Recursive Definitions"
 (scode (code:contract numbers-from : int -> list-of-int)
        (define (numbers-from n)
          (cons n (numbers-from (add1 n))))
        ...
        (define nonneg (numbers-from 0))
        (list-ref nonneg 10675)))

(slide
 #:title "Lazy Evaluation"
 (para #:aspect 'fullscreen "Languages like Plait, Java, and C are called" (dt "eager"))
 (item #:aspect 'fullscreen  "An expression is evaluated when it is encountered")
 'next
 (blank)
 (blank)
 (para #:aspect 'fullscreen  "Languages that avoid unnecessary work are called" (dt "lazy"))
 (item #:aspect 'fullscreen  "An expression is evaluated only if its result is needed"))

;; ----------------------------------------
(slide (titlet "Part 2"))

(slide
 #:title "Lazy Evaluation in Plait"
 'alts
 (list
  (list
   (para #:aspect 'fullscreen "Use")
   (code #,(tt "#lang") plait #:lazy)
   (para #:aspect 'fullscreen "to run a Plait program with lazy evaluation"))
  (list
   (para #:width (* 0.9 client-w) "For coverage reports in DrRacket:")
   (para  "In the" (bt "Choose Language...") "dialog, click" (bt "Show Details")
          "and then" (bt "Syntactic test suite coverage"))
   (para #:align 'right
         (scale/improve-new-text
          (t "(Works for both eager and lazy languages)")
          0.80))
   (blank)
   (vl-append
    (* 2 (current-line-sep))
    (item #:fill? #f (colorize (t "Black") "black") "means evaluated at least once")
    (item #:fill? #f (colorize (t "Orange") "orange") "means not yet evaluated")
    (item #:fill? #f "Normal coloring is the same as all black")))))

;; ----------------------------------------
(slide (titlet "Part 3"))

(define letrec-title
  (hbl-append (code letrec) (titlet " Interepreter in Lazy Plait")))

(slide
 #:title letrec-title
 #:layout 'top
 (para #:width (get-client-w #:aspect 'fullscreen) 
       "Doesn't work because result of"
       (code set-box!) "is never used:")
 (blank)
 (sscode
  (define (interp a env)
    (type-case Exp a
      ...
      [(letrecE n rhs body)
       (let ([b (box (none))])
         (let ([new-env (extend-env
                         (bind n b)
                         env)])
           (begin
             #,(hilite (code (set-box! b (some (interp rhs new-env)))))
             (interp body new-env))))]))))

(slide
 #:title letrec-title
 #:layout 'top
 (para #:width (get-client-w #:aspect 'fullscreen) "Working implementation is more direct:")
 (blank)
 (sscode
  (define (interp a env)
    (type-case Exp a
      ...
      [(letrecE n rhs body)
       (letrec ([new-env
                 (extend-env
                  (bind n (interp rhs new-env))
                  env)])
         (interp body new-env))]))))

;; ----------------------------------------
(slide (titlet "Part 4"))

(slide
 #:title "Lazy Language"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)         -or- (code #,(nonterm "Symbol")) (blank)
        (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)))
 'next
 (blank)
 (vl-append
  (current-line-sep)
  (code {{lambda {x} 0} {+ 1 {lambda {y} 2}}} #,sym:implies 0)
  (code {{lambda {x} x} {+ 1 {lambda {y} 2}}} #,sym:implies #,(it "error")))
 'next
 (blank)
 (code {let {[x {+ 1 {lambda {y} 2}}]}
         0} #,sym:implies 0))

;; ----------------------------------------
(slide (titlet "Part 5"))

(define (app-code fun-expr arg-interp)
  (scode
   (define (interp a env)
     (type-case Exp a
       ...
       [(appE fun arg)
        (type-case Value #,fun-expr
          [(closV n body c-env)
           (interp body
                   (extend-env
                    (bind n (#,arg-interp arg env))
                    c-env))]
          [else (error 'interp "not a function")])]))))

(define fun-expr
  (code (interp fun env)))

(define-syntax red-id
  (syntax-rules ()
    [(_ id)
     (hilite (code id))]))

(define-syntax white-id
  (syntax-rules ()
    [(_ id)
     (hilite #:color "white" (code id))]))

(slide
 #:title "Implementing Laziness"
 'alts
 (list
  (list
   (item #:bullet (colorize (t "Option #1:") BlueColor)
         "Run the interpreter in " (tt "plait #:lazy") "!")
   'next
   (blank)
   (app-code fun-expr (red-id interp))
   (blank)
   (para #:fill? #f
         (code n) "never used" sym:implies
         (code interp) "call never evaluated"))
  (list
   (item #:bullet (colorize (t "Option #2:") BlueColor)
         "Use Plait and explicitly delay" 
         (code arg) "interpretation")
   'next
   (blank)
   (app-code fun-expr (red-id delay)))))

(slide
 #:title "Thunks and Bindings"
 (code (define-type Thunk
         (delay [arg : Exp]
                [env : Env]))
       code:blank
       (define-type Binding
         (bind [name : Symbol]
               [val : Thunk]))))

(define (interp-id id-clause #:force [force ghost])
  (scode
    (define (interp a env)
      (type-case Exp a
        ...
        #,id-clause
        ...
        [(appE fun arg)
         ...
         (extend-env
          (bind n (#,(red-id delay) arg env))
          c-env)
         ...]))
    code:blank
    #,(force
       (code
        (define (force [t : Thunk]) : Value
          (type-case Thunk t
            [(delay a e) (interp a e)]))))))

(define id-clause
  (code [(idE s) (#,(red-id force) (lookup s env))]))

(slide
 #:title "Implementing Laziness"
 'alts
 (list
  (list
   (interp-id (lbl-superimpose (code ...) (ghost id-clause))))
  (list
   (interp-id id-clause))
  (list
   (interp-id id-clause #:force values))))

;; ----------------------------------------
(slide (titlet "Part 6"))

(slide
 #:title "Redundant Evaluation"
 (code
  {{lambda {x} {+ {+ x x} {+ x x}}}
   {- {+ 4 5} {+ 8 9}}})
 (blank)
 (para #:aspect 'fullscreen "How many times is" (code {+ 8 9}) "evaluated?")
 'next
 (blank)
 (para #:aspect 'fullscreen
       "Since the result is always the same, we'd like to"
       "evaluate" (code {- {+ 4 5} {+ 8 9}}) "at most once"))

(define caching-title "Caching Force Results")

(slide
 #:title caching-title
 (code (define-type Thunk
         (delay [arg : Exp]
                [env : Env]
                [done : (Boxof (Optionof Value))]))))

(slide
 #:title "Fix Up Interpreter"
 (code
  (define (interp a env)
    ....
    [(appE fun arg) 
     ... (delay arg env #,(red-id (box (none)))) ...])))

(define (xpara p) (para #:width (* (get-client-w #:aspect 'fullscreen) 0.8) p))

(slide
 #:title caching-title
 (xpara
  (scode
   (define (force [t : Thunk]) : Value
     (type-case Thunk t
       [(delay a e) (interp a e)]))))
 'next
 (para #:width (* 0.9 (get-client-w #:aspect 'fullscreen)) "⇒")
 (xpara
  (scode
   (define (force [t : Thunk]) : Value
     (type-case Thunk t
       [(delay a e d)
        (type-case (Optionof Value) (unbox d)
          [(none )
           (let ([v (interp a e)])
             (begin
               (set-box! d (some v))
               v))]
          [(some v) v])])))))

;; ----------------------------------------
(slide (titlet "Part 7"))

(define term-title "Terminology")

(slide
 #:title term-title
 'next
 (para (dt "Call-by-value") "means eager")
 (para #:align 'right "Plait, Java, C, Python...")
 'next
 (blank)
 (blank)
 (para (dt "Call-by-name") "means lazy, no caching of results")
 (para #:align 'right "... which is impractical")
 'next
 (blank)
 (blank)
 (para (dt "Call-by-need") "means lazy, with caching of results")
 (para #:align 'right "Haskell, Clean..."))

(slide
 #:title term-title
 (para (dt "Normal order") "vs" (dt "Applicative order"))
 (blank)
 (para #:align 'right "... good terms to avoid"))
