#lang slideshow

(require slideshow/code
         "utils/colors.ss"
         "utils/utils.ss"
         "utils/alg.ss")

;; ----------------------------------------

(slide
 (titlet "Part 0"))

(define (align . l)
  (apply item #:bullet (ghost bullet) l))

(define fae-1 (code _rhs))
(define fae-2 (code _body))
(define id-1 (code _name))

(define mk-rec (text  "mk-rec" `(italic . modern) (current-font-size)))

(define (mk-rec-slide)
  (slide
   #:title "Defining Recursion by Expansion"
   (align
    (code {letrec {[#,id-1 #,fae-1]}
            #,fae-2}))
   (blank)
   (para #:width (get-client-w #:aspect 'fullscreen) "could be parsed the same as")
   (blank)
   (align
    (code {let {[#,id-1 {#,mk-rec {lambda {#,id-1} #,fae-1}}]}
            #,fae-2}))
   (blank)
   (para #:width (get-client-w #:aspect 'fullscreen) "which is really")
   (blank)
   (align
    (code {{lambda {#,id-1} #,fae-2}
           {#,mk-rec {lambda {#,id-1} #,fae-1}}}))))

(mk-rec-slide)

;; ----------------------------------------

(slide
 (titlet "Part 1")
 (titlet "Metacircular Recursion"))

;; ----------------------------------------

(define loop-title "Recursive Binding")

(define (loop-slide p r #:prefixes [prefixes null])
  (slide
   #:title loop-title
   'alts
   (list prefixes)
   p
   'next
   (blank)
   r))

(define (para* . content)
  (apply para #:align 'center content))

(define (et s)
  (colorize (it s) "red"))

(loop-slide
 (code (local [(define x 10)]
         (+ x 1)))
 (para* (tt "11")))

(define infinite-loop
  (para* "infinite loop" (colorize (t "– good!") "forestgreen")))

(loop-slide
 (code (local [(define (f x)
                 (f x))]
         (f 1)))
 (para* infinite-loop))

(loop-slide
 (code (local [(define x x)]
         x))
 (para* (et "x: cannot use before initialization")))
         
(loop-slide
 (code (local [(define x (list x))]
         x))
 (para* (et "x: cannot use before initialization")))
         
(loop-slide
 (code (local [(define f 
                 (lambda (x) (f x)))]
         (f 1)))
 (para* infinite-loop))

(loop-slide
 (code (letrec ([f (lambda (x) (f x))])
         (f 1)))
 (para* infinite-loop))

(loop-slide
 (code (letrec ([f 
                 (list
                  (lambda (x) ((first f) x)))])
         ((first f) 1)))
 (para* infinite-loop))

(loop-slide
 #:prefixes (list 'alts
                  (list
                   (list
                    (code {letrec {[x 10]}
                            {+ x 1}}))
                   (list
                    (code {letrec {[f {lambda {x}
                                        {f x}}]}
                            {f 1}})))
                  'next
                  (blank))
 (code (letrec ([val (interp (lamE 'x (appE (idE 'f)
                                            (idE 'x)))
                             (extend-env (bind 'f val)
                                         env))])
         (interp (appE (idE 'f) (numE 1))
                 (extend-env (bind 'f val)
                             env))))
 (para* (et "val: cannot use before initialization")))

(loop-slide
 #:prefixes (list (code {letrec {[f {lambda {x}
                                      {f x}}]}
                          {f 1}})
                  (blank))
 (code (letrec ([new-env (extend-env (bind 'f (lambda ()
                                                val))
                                     env)]
                [val (interp (lamE 'x (appE (idE 'f)
                                            (idE 'x)))
                             new-env)])
         (interp (appE (idE 'f) (numE 1)) new-env)))
 (para* "works!"))

;; ----------------------------------------

(define meta-recur-title (hbl-append (titlet "Metacircular ") (code letrec)))

(slide
 #:title meta-recur-title
 (code
  (define-type Binding
    (bind [name : Symbol]
          [val : (#,(hilite (code ->)) Value)]))
  code:blank
  (define (lookup [n : Symbol] [env : Env]) : Value
    (type-case (Listof Binding) env
      [empty (error 'lookup "free variable")]
      [(cons b rst-env) (cond
                          [(symbol=? n (bind-name b))
                           #,(hbl-append (hilite (tt "("))
                                         (code (bind-val b))
                                         (hilite (tt ")")))]
                          [else (lookup n rst-env)])]))))

;; ----------------------------------------

(slide
 (titlet "Part 2"))

(slide
 #:title "Expr Grammar"
 (grammar-table
  (list (nonterm "Exp")  eqls (nonterm "Number") (blank)
        (blank)          -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {- #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code #,(nonterm "Symbol")) (blank)
        (blank)          -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {let {[#,(nonterm "Symbol") #,(nonterm "Exp")]} #,(nonterm "Exp")}) (blank)
        (blank)          -or- (code {if0 #,(nonterm "Exp") #,(nonterm "Exp") #,(nonterm "Exp")}) new-label
        (blank)          -or- (code {letrec {[#,(nonterm "Symbol") #,(nonterm "Exp")]} #,(nonterm "Exp")}) new-label)))

;; ----------------------------------------

(slide
 #:title meta-recur-title
 (scale
  (code
   (define (interp [a : Expr] [env : Env]) : Value
     (type-case Expr a
       ....
       [(letrecE n rhs body)
        (letrec ([new-env
                  (extend-env
                   (bind n (lambda () val))
                   env)]
                 [val
                  (interp rhs new-env)])
          (interp body new-env))])))
  0.9))

(slide
 #:title meta-recur-title
 (code
  (test (interp (parse `{letrec {[fac
                                  {lambda {x}
                                    {if0 x
                                         1
                                         {* x {fac {+ x -1}}}}}]}
                          {fac 5}})
                mt-env)
        (numV 120))))

(slide
 #:title meta-recur-title
 (code
  (interp (parse `{letrec {[x x]}
                    x})
          mt-env))
 'next
 (colorize (it "val: cannot use before initialization") "red")
 'next
 (blank)
 (para #:align 'right "... and cannot capture with" (code test/exn)))
 
;; ----------------------------------------

(slide
 (titlet "Part 3")
 (titlet "Assignment-Based Recursion"))

(mk-rec-slide)

;; ----------------------------------------

(slide
 #:title "Defining Recursion by Expansion"
 (para #:width client-w "Another approach:")
 (align
  (scode (letrec ([fac
                   (lambda (n)
                     (if (zero? n)
                         1
                         (* n (fac (- n 1)))))])
           (fac 10))))
 (para sym:implies)
 (align
  (scode (let ([fac 42])
           (begin
             (set! fac
                   (lambda (n)
                     (if (zero? n)
                         1
                         (* n (fac (- n 1))))))
             (fac 10))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Implementing Recursion"
 (para "The" (code set!) "approach to definition works only"
       "when the defined language includes" (code set!))
 (blank)
 (para "But the" (code set!) "approach to implementation"
       "requires only that the implementation language includes"
       (code set!) "..."))

(define assign-recur-title (hbl-append (titlet "Assignment-Based ") (code letrec)))

(slide
 #:title assign-recur-title
 (code
  (define-type Binding
    (bind [name : Symbol]
          [val : (#,(hilite (code Boxof)) Value)]))
  code:blank
  (define (lookup [n : Symbol] [env : Env]) : Value
    (type-case (Listof Binding) env
      [empty (error 'lookup "free variable")]
      [(cons b rst-env) (cond
                          [(symbol=? n (bind-name b))
                           (#,(hilite (code unbox)) (bind-val b))]
                          [else (lookup n rst-env)])]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rcfae-short-code rec-code)
  (para
   #:width (get-client-w #:aspect 'fullscreen)
   (code
    (define (interp [a : Expr] [env : Env]) : Value
      (type-case Expr a
        ....
        [(letrecE n rhs body)
         #,rec-code])))))

(define final-42-impl
  (code
   (let ([b (box (numV 42))])
     (let ([new-env (extend-env
                     (bind n b)
                     env)])
       (begin
         (set-box! b (interp rhs new-env))
         (interp body new-env))))))

(slide
 #:title assign-recur-title
 'alts~
 (list
  (list (rcfae-short-code (code ...)))
  (list (rcfae-short-code (code
                           ... (interp rhs env)
                           ... (interp body env) ...)))
  (list (rcfae-short-code (code
                           ... (interp rhs new-env)
                           ... (interp body new-env) ...)))
  (list (rcfae-short-code (code
                           (let ([new-env (extend-env
                                           (bind n ...)
                                           env)])
                             ... (interp rhs new-env)
                             ... (interp body new-env) ...))))
  (list (rcfae-short-code (code
                           (let ([new-env (extend-env
                                           (bind n (box ...))
                                           env)])
                             ... (interp rhs new-env)
                             ... (interp body new-env) ...))))
  (list (rcfae-short-code (code
                           (let ([new-env (extend-env
                                           (bind n (box (numV 42)))
                                           env)])
                             ... (interp rhs new-env)
                             ... (interp body new-env) ...))))
  (list (rcfae-short-code (code
                           (let ([b (box (numV 42))])
                             (let ([new-env (extend-env
                                             (bind n b)
                                             env)])
                               ... (interp rhs new-env)
                               ... (interp body new-env) ...)))))
  (list (rcfae-short-code (code
                           (let ([b (box (numV 42))])
                             (let ([new-env (extend-env
                                             (bind n b)
                                             env)])
                               ... (set-box! b (interp rhs new-env))
                               ... (interp body new-env) ...)))))
  (list (rcfae-short-code final-42-impl))))

;; ----------------------------------------

(slide
 (titlet "Part 4")
 (titlet "Use Before Initialization"))

(define use-before-title "Use Before Initialization")

(slide
 #:title use-before-title
 (para
  #:width (* 0.7 (get-client-w #:aspect 'fullscreen))
  (code
   (interp (parse `{letrec {[x x]}
                     x})
           mt-env)))
 'next
 (para
  #:width (* 0.7 (get-client-w #:aspect 'fullscreen))
  sym:implies (code (numV 42))))

(slide
 #:title use-before-title
 (rcfae-short-code final-42-impl))

(slide
 #:title use-before-title
 (code
  (define-type Binding
    (bind [name : Symbol]
          [val : (Boxof (#,(hilite (code Optionof)) Value))]))
  code:blank
  (define (lookup [n : Symbol] [env : Env]) : Value
    (type-case (Listof Binding) env
      [empty (error 'lookup "free variable")]
      [(cons b rst-env)
       (cond
         [(symbol=? n (bind-name b))
          #,(let-syntax ([type-case (make-code-transformer
                                     (lambda (stx)
                                       (syntax-case stx ()
                                         [(tc . rest)
                                          (with-syntax ([tc (syntax/loc #'tc
                                                              #,(hilite (code type-case)))])
                                            #`(code #,(syntax/loc stx (tc . rest))))])))])
              (code
               (type-case #,(hilite (code (Optionof Value))) (unbox (bind-val b))
                 [(none) (error 'lookup "use before initialization")]
                 [(some v) v])))]
         [else (lookup n rst-env)])]))))

(slide
 #:title use-before-title
 (rcfae-short-code 
  (code
   (let ([b (box (none))])
     (let ([new-env (extend-env
                     (bind n b)
                     env)])
       (begin
         (set-box! b (some (interp rhs new-env)))
         (interp body new-env)))))))

;; ----------------------------------------

(slide
 (titlet "Part 5")
 (titlet "Cyclic Data"))

(define (add-cycle-arrow p self)
  (refocus
   (pin-arrow-line (/ gap-size 2)
                   p
                   self cb-find
                   p (lambda (p q)
                       (define-values (x y) (lt-find p q))
                       (values x (+ y (pict-height self))))
                   #:start-angle (* pi -1/2)
                   #:end-angle (* pi 1/4)
                   #:color "orange"
                   #:line-width 2)
   p))

(slide
 #:title "Cycles"
 'alts
 (list
  (list
   (code
    (letrec ([f #,(let ([f (code f)])
                    (add-cycle-arrow
                     (code (lambda (x)
                             (#,f x)))
                     f))])
      ...)))
  (list
   (let ([hole (frame (ghost (tt "f")))])
     (add-cycle-arrow
      (code (closV 'x
                   (appE (idE 'f) (idE 'x))
                   (extend-env
                    (bind 'f (box #,hole))
                    mt-env)))
      hole)))))
