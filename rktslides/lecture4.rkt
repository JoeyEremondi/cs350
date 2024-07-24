#lang slideshow
(require slideshow/code
         slideshow/play
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss")

(fullscreen-para-width!)

(define (align . args)
  (apply para #:width (get-client-w #:aspect 'fullscreen) args))
(define (nalign . args)
  (apply para args))

;; ----------------------------------------
(slide (titlet "Part 1"))
  
(slide
 #:title "Values"
 (para "A" (bit "value") "is the result of an" (bit "expression"))
 (item "Expression: " (code {+ 1 2}))
 (item "Value: " (code 3))
 'next
 (blank)
 (para
  #:align 'right
  (vl-append
   (current-line-sep)
   (para #:fill? #f "A value can be be")
   (para #:fill? #f " the argument to a function,")
   (para #:fill? #f " the right-hand side of a" (code let) ",")
   (para #:fill? #f " ..."))))

(slide
 #:title "Functions as Values?"
 (para "Is a function a value in Curly?")
 'next
 (blank)
 (para (colorize (bt "No") "red"))
 'next
 (blank)
 (para "You can define a function")
 (code {define {double x} {+ x x}})
 (para "You can call a function")
 (code {double 10})
 'next
 (blank)
 (para "You" (it "cannot") "use a function name without calling it")
 (para "You" (it "cannot") "pass a function to another function"))

(slide
 #:title "Functions as Values?"
 (para "Is a function a value in Plait?")
 'next
 (blank)
 (para (colorize (bt "Yes") "forestgreen"))
 'next
 (blank)
 (para "An expression can produce a function result")
 (code (define (double x) (+ x x))
       double)
 'next
 (code (list + * - /))
 'next
 (code (lambda (x) (+ x x)))
 'next
 (blank)
 (para "You can pass a function to a function:")
 (code (map (lambda (x) (+ x x))
            (list 1 2 3))))

(slide
 #:title "Why Functions as Values"
 (para "Abstraction is easier with functions as values")
 (subitem #:bullet bullet (code filter) "," (code map) "," (code foldl) ", etc.")
 'next
 (blank)
 (para "Separate" (code define) "form becomes unnecessary")
 (vl-append
  (code {define {f x} {+ 1 x}}
        {f 10})
  (code code:blank)
  sym:implies
  (code code:blank)
  (code {let {[f {lambda {x} {+ 1 x}}]}
          {f 10}})))
  
;; ----------------------------------------
(slide (titlet "Part 2"))
  
(slide
 #:title "New Curly Grammar, Almost"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)         -or- (code #,(nonterm "Symbol")) (blank)
        (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {let {[#,(nonterm "Symbol") #,(nonterm "Exp")]} #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {#,(nonterm "Symbol") #,(nonterm "Exp")}) (colorize (bt "*") RedColor)
        (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) new-label)))

(define produces (hbl-append (tt " ") sym:implies (tt " ")))

(define neg-space (blank 0 (- (current-line-sep) (* 2 gap-size))))

(parameterize ([current-para-width (* 0.9 (get-client-w #:aspect 'fullscreen))])
  (slide
   #:title "Evaluation"
   (para (code 10) produces (code 10))
   (para (code y) produces (it "free variable"))
   (para (code {+ 1 2}) produces (code 3))
   (para (code {* 2 3}) produces (code 6))
   (para (code {let {[x 7]} {+ x 2}}) produces (code {+ 7 2}) produces (code 9))
   'next
   'alts~
   (list
    (list (para (code {lambda {x} {+ 1 x}}) produces))
    (list (para (code {lambda {x} {+ 1 x}}) produces (code {lambda {x} {+ 1 x}}))))
   'next
   'alts
   (list (list (blank)
               (colorize (para #:align 'center "Result is not always a number!") BlueColor)
               (strike-out (code (code:contract interp Exp ... -> Number)))
               (code (code:contract interp Exp ... -> Value)))
         (list 
          (para (code {let {[y 10]} {lambda {x} {+ y x}}}))))
   neg-space
   'alts~
   (list
    (list (para produces))
    (list (para produces (code {lambda {x} {+ 10 x}}))))
   'next
   'alts~
   (list
    (list
     (para (code {let {[f {lambda {x} {+ 1 x}}]} {f 3}}))
     neg-space
     'alts~
     (list
      (list (para produces))
      (list (para produces (code {{lambda {x} {+ 1 x}} 3})))))
    (let ([open-close (lambda (p)
                        (ltl-superimpose
                         (hilite #:color "lightgreen" (ghost (tt "{")))
                         (rtl-superimpose
                          (hilite #:color "lightgreen" (ghost (tt "}")))
                          (pin-over (code {#,(ghost p)})
                                    p lt-find
                                    p))))])
      (list
       (para (code {let {[f #,(hilite (code {lambda {x} {+ 1 x}}))]} #,(open-close (code (code:line f 3)))}))
       neg-space
       (para produces (open-close (code (code:line #,(hilite (code {lambda {x} {+ 1 x}})) 3)))))))
   'next
   (colorize (para #:align 'center "Doesn't match the grammar for" (nonterm "Exp")) BlueColor)))

(slide
 #:title "New Curly Grammar"
 (let ([blank-tag (tt " ")])
   (delete-grammar-line
    (grammar-table
     (list (nonterm "Exp") eqls (nonterm "Number") (blank)
           (blank)         -or- (code #,(nonterm "Symbol")) (blank)
           (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
           (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
           (blank)         -or- (code {let {[#,(nonterm "Symbol") #,(nonterm "Exp")]} #,(nonterm "Exp")}) (blank)
           (blank)         -or- (code {#,(nonterm "Symbol") #,(nonterm "Exp")}) blank-tag
           (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) new-label
           (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) new-label))
    blank-tag
    (nonterm "Exp")
    new-label)))
  
(slide
 #:title "Evaluation"
 (para (code {let {[f {lambda {x} {+ 1 x}}]} {f 3}}))
 neg-space
 (para produces (code {{lambda {x} {+ 1 x}} 3}))
 neg-space
 (para produces (code {+ 1 3}) produces (code 4))
 'next
 (para (code {{lambda {x} {+ 1 x}} 3}) produces (code {+ 1 3}) produces (code 4))
 'next
 'alts~
 (list (list (para (code {1 2}) produces))
       (list (para (code {1 2}) produces (it "not a function"))))
 'next
 'alts~
 (list (list (para (code {+ 1 {lambda {x} 10}}) produces))
       (list (para (code {+ 1 {lambda {x} 10}}) produces (it "not a number")))))

;; ----------------------------------------
(slide (titlet "Part 3"))
  
(slide
 #:title "Expression Datatype"
 (scale
  (scode
   (define-type Exp
     (numE [n : Number])
     (idE [s : Symbol])
     (plusE [l : Exp] 
            [r : Exp])
     (multE [l : Exp]
            [r : Exp])
     (letE [n : Symbol] 
           [rhs : Exp]
           [body : Exp])
     (lamE [n : Symbol]
           [body : Exp])
     (appE [fun : Exp]
           [arg : Exp])))
  0.95)
 'next
 (blank)
 'alts
 (list (list
        (scode (test (parse `{lambda {x} {+ x 1}})
                     (lamE 'x (plusE (idE 'x) (numE 1))))))
       (list
        (scode (test (parse `{{lambda {x} {+ x 1}} 10})
                     (appE (lamE 'x (plusE (idE 'x) (numE 1))) 
                           (numE 10)))))))
         
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(slide (titlet "Part 4"))

(define (subst-slide #:hilite? [hilite? #t]
                     #:result? [result? #t]
                     #:move-n [move-n 0])
  (define body
    (hilite #:on? hilite? (code {lambda {x} {+ y x}})))
  (define g-body1 (launder (ghost body)))
  (define g-body2 (launder (ghost body)))
  (define two (code {let {[y 10]}
                      #,g-body1}))
  (define one (code {let {[y 10]} #,g-body2}))
  (define res (if result? values ghost))
  (vl-append
   gap-size
   (code (interp #,(parsed #,(slide-pict
                              (fade-pict move-n
                                         #:combine ltl-superimpose
                                         two
                                         one)
                              body
                              g-body1
                              g-body2
                              move-n))))
   (res sym:implies)
   (res (code {lambda {x} {+ 10 x}}))))

(define subst-title "Functions with Substitutions")

(slide
 #:title subst-title
 (subst-slide #:hilite? #f #:result? #f))
(slide
 #:title subst-title
 (subst-slide #:result? #f))
(play-n #:title subst-title
        (lambda (n) (subst-slide #:move-n n)))

(slide
 #:title "Functions with Deferred Substitution"
 (align (code (interp #,(parsed+sub
                         {let {[y 10]} {lambda {x} {+ y x}}}
                         code:blank))))
 'next (align sym:implies)
 (align (code (interp #,(parsed+sub
                         {lambda {x} {+ y x}}
                         (code:line y = 10)))))
 #;
 (
  'next 
  (blank)
  (blank)
  (blank)
  (blank)
  (align (code (interp #,(parsed+sub
                          {{lambda {y} {lambda {x} {+ y x}}} 10}
                          code:blank))))
  'next (align sym:implies)
  (align (code (interp #,(parsed+sub
                          {lambda {x} {+ y x}}
                          (code:line y = 10)))))))

(define (in-align . x)
  (apply item #:bullet (ghost bullet) #:width (get-client-w #:aspect 'fullscreen) x))
(define fun-align in-align)
(define arg-align in-align)
  
(define (gen-app f? a?)
  (list
   (align (code (interp #,(parsed+sub
                           {#,(hilite #:on? f? (code {let {[y 10]} {lambda {x} {+ y x}}}))
                            #,(hilite #:on? a? (code {let {[y 7]} y}))}
                           code:blank))))))

(slide
 #:title "Functions with Deferred Substitution"
 'alts~
 (list
  (gen-app #f #f)
  (gen-app #t #f)
  (gen-app #f #t)
  (gen-app #f #f))
 (inset (colorize (arg-align "Argument expression:") BlueColor) 0 0 0 (- 1 gap-size))
 (arg-align (code (interp #,(parsed+sub
                             {let {[y 7]} y}
                             code:blank))))
 (arg-align sym:implies)
 (arg-align (code (interp #,(parsed+sub
                             y
                             (code:line y = 7))))
            produces (code 7))
 'next 
 (inset (colorize (fun-align "Function expression:") BlueColor) 0 0 0 (- 1 gap-size))
 (fun-align (code (interp #,(parsed+sub
                             {let {[y 10]} {lambda {x} {+ y x}}}
                             code:blank))))
 (fun-align sym:implies)
 'alts
 (let ([lhs (code (interp #,(parsed+sub
                             {lambda {x} {+ y x}}
                             (code:line y = 10))))]
       [rhs (colorize (bt "?") RedColor)])
   (list
    (list
     (fun-align lhs produces rhs))
    (list
     (fun-align lhs produces (refocus (hc-append (* 3 gap-size)
                                                 rhs
                                                 (para #:width (/ client-w 4)
                                                       "A" (bit "closure") "combines"
                                                       "an expression with an environment"))
                                      rhs))))))

(define test-case
  (un-s-scale
   (scode (interp #,(parsed {let {[y 10]} {lambda {x} {+ y x}}})
                  mt-env))))

(slide
 #:title "Representing Values"
 (scode (define-type Value
          (numV [n : Number])
          (closV [arg : Symbol]
                 [body : Exp]
                 [env : Env]))
        code:blank
        (define-type Binding
          (bind [name : Symbol]
                [val : Value])))
 'next
 (blank)
 'alts~
 (list (list (nalign (scode (test (interp #,(parsed 10) mt-env)
                                  ...))))
       (list (nalign (scode (test (interp #,(parsed 10) mt-env)
                                  (numV 10)))))
       (list (scode (test #,test-case
                          (closV ... ... ...))))
       (list (scode (test #,test-case
                          (closV 'x #,(parsed {+ y x})
                                 (extend-env (bind 'y (numV 10))
                                             mt-env)))))))

(slide
 #:title "Continuing Evaluation"
 (vl-append
  gap-size
  (htl-append
   (t "Argument: ")
   (vl-append 
    (current-line-sep)
    (code (interp #,(parsed+sub y (code:line y = (numV 7)))))
    (htl-append gap-size sym:implies
                (code (numV 7)))))
  (htl-append
   (t "Function: ")
   (vl-append
    (current-line-sep)
    (code (interp #,(parsed+sub {lambda {x} {+ y x}} (code:line y = (numV 10)))))
    (htl-append
     gap-size
     sym:implies (code
                  (closV 'x #,(parsed {+ y x})
                         (extend-env (bind 'y (numV 10))
                                     mt-env)))))))
 'next
 (blank)
 (blank)
 (para #:width client-w "To apply, interpret the function body with the given argument:")
 (blank)
 'alts~
 (list (list (nalign (code (interp ...))))
       (list (nalign (code (interp #,(parsed+sub
                                      {+ y x}
                                      ...)))))
       (list (nalign (code (interp #,(parsed+sub
                                      {+ y x}
                                      (code:line x = (numV 7) #,(tt "") ...))))))
       (list (nalign (code (interp #,(parsed+sub
                                      {+ y x}
                                      (code:line x = (numV 7) #,(tt "") y = (numV 10)))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(slide (titlet "Part 5"))

(define let-code
  (code (interp body (extend-env
                      (bind n (interp rhs env))
                      env))))

(define (interp-code #:step [step 0]
                     #:num-code [num-code (if (step . > . 0)
                                              (code (numV n))
                                              (code ...))]
                     #:id-code [id-code (if (step . > . 1)
                                            (code (lookup s env))
                                            (code ...))]
                     #:add-code [add-code (if (step . > . 2)
                                              (code (num+ (interp l env) (interp r env)))
                                              (code ...))]
                     #:mult-code [mult-code (if (step . > . 3)
                                                (code (num* (interp l env) (interp r env)))
                                                (code ...))]
                     #:let-code [let-code (if (step . > . 4)
                                              let-code
                                              (code ...))]
                     #:fun-code [fun-code (if (step . > . 5)
                                              (code (closV n body env))
                                              (code ...))]
                     #:app-code [app-code (code ...)])
  (align
   (scale
    (code
     (define (interp [a : Exp] [env : Env]) : Value
       (type-case Exp a
         [(numE n) #,(hilite #:on? (= step 0) num-code)]
         [(idE s) #,(hilite #:on? (= step 1) id-code)]
         [(plusE l r) #,(hilite #:on? (= step 2) add-code)]
         [(multE l r) #,(hilite #:on? (= step 3) mult-code)]
         [(letE n rhs body)
          #,(hilite #:on? (= step 4) let-code)]
         [(lamE n body) #,(hilite #:on? (= step 5) fun-code)]
         [(appE fun arg)
          #,(hilite #:on? (= step 6) app-code)])))
    0.85)))

(define closure-code (code (closV param body-expr env)))

(define interp-title "Interpreter")

(slide
 #:title interp-title
 #:layout 'top
 'alts~
 (list (list (interp-code #:step -1))
       (list (interp-code #:step 0))
       (list (interp-code #:step 0 #:num-code (code ... n ...)))
       (list (interp-code #:step 0 #:num-code (code (numV n))))
       (list (interp-code #:step 1))
       (list (interp-code #:step 1 #:id-code (code (lookup s env))))
       (list (interp-code #:step 2))
       (list (interp-code #:step 2 #:add-code (code (interp l env) ... (interp r env))))
       (list (interp-code #:step 2 #:add-code (code (#,(hilite (code +) #:color "pink") (interp l env) (interp r env)))))
       (list (interp-code #:step 2 #:add-code (code (num+ (interp l env) (interp r env)))))))

(parameterize ([current-para-width (* 0.9 (get-client-w #:aspect 'fullscreen))])  
  (slide
   #:title "Add and Multiply"
   'alts
   (list
    (list
     (para
      (scode
       (define (num+ [l : Value] [r : Value]) : Value
         (cond
          [(and (numV? l) (numV? r))
           (numV (+ (numV-n l) (numV-n r)))]
          [else
           (error 'interp "not a number")]))
       code:blank
       (define (num* [l : Value] [r : Value]) : Value
         (cond
          [(and (numV? l) (numV? r))
           (numV (* (numV-n l) (numV-n r)))]
          [else
           (error 'interp "not a number")])))))
    (list
     (para
      (scode
       (define (num-op op l r)
         (cond
          [(and (numV? l) (numV? r))
           (numV (op (numV-n l) (numV-n r)))]
          [else
           (error 'interp "not a number")]))
       code:blank
       (define (num+ [l : Value] [r : Value]) : Value
         (num-op + l r))
       code:blank
       (define (num* [l : Value] [r : Value]) : Value
         (num-op * l r))))))))

(define (app-code #:c-env? [c-env? #f]
                  #:extend? [extend? c-env?]
                  #:bind? [bind? extend?]
                  #:interp-body? [interp-body? bind?]
                  #:error? [error? interp-body?]
                  #:type-case? [type-case? error?])
  (define (choose on? a b)
    (if on?
        a
        (refocus
         (lt-superimpose (ghost a)
                         b)
         a)))
  (define interp-fun (code (interp fun env)))
  (define interp-arg (code (interp arg env)))
  (define bind (choose
                bind?
                (code (bind n #,(ghost interp-arg)))
                (code ...)))
  (define c-env (choose
                 c-env?
                 (code c-env)
                 (code ...)))
  (define extend (choose
                  extend?
                  (code (extend-env
                         #,(ghost bind)
                         #,(ghost c-env)))
                  (code ...
                        code:blank
                        ...)))
  (define interp-body (choose
                       interp-body?
                       (code (interp body
                                    #,(ghost extend)))
                       (code ...)))
  (define error (choose
                 error?
                 (code (error 'interp "not a function"))
                 (code ...)))
  (define type-case (choose
                     type-case?
                     (code (type-case Value #,(ghost interp-fun)
                             [(closV n body c-env)
                              #,(ghost interp-body)]
                             [else #,(ghost error)]))
                     (code ...
                           code:blank
                           code:blank
                           ...)))
  (for/fold ([p type-case]) ([s (list error
                                      interp-body
                                      extend
                                      c-env
                                      bind
                                      interp-arg
                                      interp-fun)])
    (pin-over p s lt-find s)))

(define final-app-code (app-code #:c-env? #t))

(slide
 #:title interp-title
 #:layout 'top
 'alts~
 (list (list (interp-code #:step 2 #:add-code (code (num+ (interp l env) (interp r env)))))
       (list (interp-code #:step 3 #:mult-code (code (num* (interp l env) (interp r env)))))
       (list (interp-code #:step 4))
       (list (interp-code #:step 4 #:let-code let-code))
       (list (interp-code #:step 5))
       (list (interp-code #:step 5 #:fun-code (code ... (interp body env) ...)))
       (list (interp-code #:step 5 #:fun-code (code ... (#,(strike-out (code interp)) body env) ...)))
       (list (interp-code #:step 5 #:fun-code (code ... (closV ... body env) ...)))
       (list (interp-code #:step 5 #:fun-code (code ... (closV n body env) ...)))
       (list (interp-code #:step 5 #:fun-code (code (closV n body env))))
       (list (interp-code #:step 6))
       (list (interp-code #:step 6 #:app-code (app-code)))
       (list (interp-code #:step 6 #:app-code (app-code #:type-case? #t)))
       (list (interp-code #:step 6 #:app-code (app-code #:error? #t)))
       (list (interp-code #:step 6 #:app-code (app-code #:interp-body? #t)))
       (list (interp-code #:step 6 #:app-code (app-code #:bind? #t)))
       (list (interp-code #:step 6 #:app-code (app-code #:extend? #t)))
       (list (interp-code #:step 6 #:app-code final-app-code))
       (list (interp-code #:step 7 #:app-code final-app-code))))
