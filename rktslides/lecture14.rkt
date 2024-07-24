#lang slideshow
(require slideshow/code
         "utils/colors.ss"
         "utils/utils.ss"
         "utils/alg.ss"
         "utils/types.ss"
         "utils/fae-types.ss")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 1)

(define wrap-quiz-result values)

(define (ques n . s)
  (apply item
         #:bullet (ghost bullet)
         #; (colorize (t (format "Question #~a:" n)) GreenColor)
         s))

(define (wrong-ans . s)
  (wrap-quiz-result
   (item
    #:bullet (ghost bullet)
    (colorize (t "Wrong answer:")  RedColor)
    s)))

(define (ans . s)
  (wrap-quiz-result
   (apply item
          #:bullet (ghost bullet)
          (colorize (t "Answer:")  BlueColor)
          (if (and (= 1 (length s))
                   (string? (car s)))
              (list (bt (car s)))
              s))))

(define (quiz-slide . l)
  (apply slide 
         #:title "Expressions and Values"
         #:layout 'top
         l))

(quiz-slide
 (ques 1 "What is the value of the following expression?")
 (code {+ 1 2})
 'next
 (blank)
 (wrong-ans (bt "0"))
 'next
 (wrong-ans (bt "42"))
 'next
 (blank)
 (ans (bt "3")))

(quiz-slide
 (ques 2 "What is the value of the following expression?")
 (code {+ lambda 17 8})
 'next
 (blank)
 (wrong-ans (bt "error"))
 'next
 (blank)
 (ans "Trick question!"
      (code {+ lambda 17 8})
      "is not an expression"))

(define quiz-grammar
  (grammar-table
   (list (nonterm "Exp") eqls (nonterm "Number") (blank)
         (blank)           -or- (code #t) (blank)
         (blank)           -or- (code #f) (blank)
         (blank)           -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {= #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code #,(nonterm "Symbol")) (blank)
         (blank)           -or- (code {lambda {#,(repeat (nonterm "Symbol"))} #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {#,(nonterm "Exp") #,(repeat (nonterm "Exp"))}) (blank)
         (blank)           -or- (code {if #,(nonterm "Exp") #,(nonterm "Exp") #,(nonterm "Exp")}) (blank))))

(slide
 #:title "Language Grammar"
 quiz-grammar)

(quiz-slide
 (ques 3 "Is the following an expression?")
 (code {{lambda {} 1} 7})
 'next
 (blank)
 (wrong-ans (bt "No"))
 'next
 (blank)
 (ans (bt "Yes") "(according to our grammar)")
 'next
 (scale quiz-grammar 0.75))

(quiz-slide
 (ques 4 "What is the value of the following expression?")
 (code {{lambda {} 1} 7})
 'next
 (blank)
 (ans (bt "1") "(according to some interpreters)")
 'next
 (blank)
 (wrap-quiz-result
  (para "But no real language would accept"
        (code {{lambda {} 1} 7})))
 'next
 (blank)
 (para "Let's agree to call" (code {{lambda {} 1} 7})
       "an" (dt "ill-formed expression") ", because"
       (code {lambda {} 1}) "should be used with only"
       "zero arguments")
 (para "Let's agree to never evaluate"
       "ill-formed expressions"))

(quiz-slide
 (ques 5 "What is the value of the following expression?")
 (code {{lambda {} 1} 7})
 'next
 (blank)
 (ans (bt "None") " --- the expression is ill-formed"))

(quiz-slide
 (ques 6 "Is the following a well-formed expression?")
 (code {+ {lambda {} 1} 8})
 'next
 (blank)
 (ans "Yes"))

(quiz-slide
 (ques 7 "What is the value of the following expression?")
 (code {+ {lambda {} 1} 8})
 'next
 (blank)
 (ans (bt "None") " --- it produces an error:")
 (parameterize ([current-main-font `(italic . ,(current-main-font))])
   (wrap-quiz-result
    (para #:fill? #f
          #:width (* 2/3 (get-client-w #:aspect 'fullscreen))
          "interp: not a number")))
 'next
 (blank)
 (para "Let's agree that a" (code lambda) "expression cannot be"
       "inside a" (code +) "form"))

(quiz-slide
 (ques 8 "Is the following a well-formed expression?")
 (code {+ {lambda {} 1} 8})
 'next
 (blank)
 (ans "No"))

(quiz-slide
 (ques 9 "Is the following a well-formed expression?")
 (code {+ {{lambda {x} x} 7} 5})
 'next
 (blank)
 (ans "Depends on what we meant by" (it "inside")
      "in our most recent agreement")
 (wrap-quiz-result
  (subitem (it "Anywhere inside") " --- " (bt "No")))
 (wrap-quiz-result
  (subitem (it "Immediately inside") " --- " (bt "Yes")))
 'next
 (blank)
 (para "Since our intrepreter produces" (bt "12") 
       ", and since that result makes sense,"
       "let's agree on" (it "immediately inside")))

(quiz-slide
 (ques 10 "Is the following a well-formed expression?")
 (code {+ {{lambda {x} x} {lambda {y} y}} 5})
 'next
 (blank)
 (ans (bt "Yes") ", but we don't want it to be!"))

(quiz-slide
 (ques 11
       "Is it possible to define" 
       (bit "well-formed")
       "(as a decidable property)"
       "so that we reject all expressions that produce errors?")
 'next
 (blank)
 (ans (bt "Yes") ": reject" (it "all") "expressions!"))

(quiz-slide
 (ques 12 
       "Is it possible to define" 
       (bit "well-formed")
       "(as a decidable property)"
       "so that we reject"
       (it "only")
       "expressions that produce errors?")
 'next
 (blank)
 (ans (bt "No"))
 'next
 (blank)
 (wrap-quiz-result
  (code {+ 1 {if ... 1 {lambda {x} x}}}))
 (blank)
 (wrap-quiz-result
  (para "If we always knew whether" (code ...)
        "produces true or false, we could solve the halting problem")))

;; ----------------------------------------
(part 2)

(slide
 #:title "Types"
 (para "We cannot reject" (it "only") "bad programs")
 'next
 (blank)
 (para "In the process of rejecting expressions"
       "that are certainly bad, also reject some"
       "expressions that are good")
 (code {+ 1 {if {prime? 131101} 
                1 
                {lambda {x} x}}})
 'next
 (blank)
 (para "Overall strategy:")
 (item "Assign a" (bit "type") "to each expression"
       (it "without evaluating"))
 (item "Compute the type of a complex expression based on the types of its subexpressions"))


(define (basic-types type-tree rules)
  (slide
   #:title (if rules "Type Rules" "Types")
   #:layout (if rules 'top 'center)
   (or rules 'nothing)
   (if rules 'next 'nothing)
   (if rules (blank) 'nothing)
   (hbl-append (code 1 : _num))
   (hbl-append (code #t : _bool))
   'next
   (blank)
   'alts~
   (type-tree _ (hbl-append (open-paren) (code +) __) (code 1) __ (code 2) #f #f (close-paren) _
              (code _num) (code _num) #f (code _num) #f vc-append)
   'next
   (blank)
   'alts~
   (type-tree _ (hbl-append (open-paren) (code +) __) (code 1) __ (code #f) #f #f (close-paren) _
              (code _num) (code _bool) #f no-type #f vc-append)))

(basic-types type-tree #f)

(define int-rule (code #,(nonterm "Number") : _num))
(define bool-rule (vc-append (* 2 (current-line-sep))
                             (code #t : _bool)
                             (code #f : _bool)))
(define old-plus-rule (scale/improve-new-text
                       (infer (code {+ #,(subnt "Exp" 1) #,(subnt "Exp" 2)} : _num)
                              (ante-append
                               (code #,(subnt "Exp" 1) : _num)
                               (code #,(subnt "Exp" 2) : _num)))
                       0.9))

(define basic-rules (hc-append (* 3 (current-font-size))
                               (vl-append
                                (current-font-size)
                                int-rule
                                bool-rule)
                               old-plus-rule))

;; ----------------------------------------
(part 3)

(basic-types type-tree/formal basic-rules)

(slide
 #:title "Type Rules"
 #:layout 'top
 basic-rules
 (blank)
 (infer (code {+ {+ 1 2} 3} : _num)
        (ante-append
         (infer (code {+ 1 2} : _num)
                (ante-append
                 (code 1 : _num)
                 (code 2 : _num)))
         (code 3 : _num))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 4)

(define (conditional-types type-tree rules)
  (slide
   #:title (if rules "Conditional Type Rules" "Types: Conditionals")
   (or rules 'nothing)
   (if rules 'next 'nothing)
   (if rules (blank) 'nothing)
   'alts~
   (type-tree _ (hbl-append (open-paren) (code if) __) (code #t) __ (code 1) __ (code 2) (close-paren) _
              (code _bool) (code _num) (code _num) (code _num) #f vc-append)
   'next
   (blank)
   'alts~
   (type-tree _ (hbl-append (open-paren) (code if) __) (code {+ 1 2}) __ (code 1) __ (code 2) (close-paren) _
              (code _num) (code _num) (code _num) no-type #f vc-append)
   'next
   (blank)
   'alts~
   (type-tree _ (hbl-append (open-paren) (code if) __) (code #f) __ (code 1) __ (code #f) (close-paren) _
              (code _bool) (code _num) (code _bool) no-type #f vc-append)))

(conditional-types type-tree #f)

(define cond-rule 
  (scale/improve-new-text
   (infer (code {if #,(subnt "Exp" 1) #,(subnt "Exp" 2) #,(subnt "Exp" 3)} : #,(subnt "Type" 0))
          (ante-append
           (code #,(subnt "Exp" 1) : _bool)
           (code #,(subnt "Exp" 2) : #,(subnt "Type" 0))
           (code #,(subnt "Exp" 3) : #,(subnt "Type" 0))))
   0.8))

(conditional-types type-tree/formal cond-rule)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 5)

(define shrinker (lambda (l) 
                   (parameterize ([current-expected-text-scale
                                   (map (lambda (x) (* x 0.7)) (current-expected-text-scale))])
                     (map (lambda (a) (map (lambda (x) (scale x 0.7)) a)) (l)))))

(define (variable-types type-tree rules)
  (slide
   #:title (if rules
               "Variable and Function Type Rules"
               "Types: Variables and Functions")
   (or rules 'nothing)
   (if rules (blank) 'nothing)
   'alts
   ((if rules
        (lambda (l)
          (list
           (list (blank)
                 (htl-append (t "Abbreviations: ")
                             __
                             (vl-append
                              (para #:fill? #f (T) " = " (nonterm "Type"))
                              (para #:fill? #f (M) " = " (nonterm "Exp"))
                              (para #:fill? #f (E) " = " (nonterm "Env")))))
           l))
        list)
    (list (hbl-append (if rules 
                          (code #,(mtenv) #,(ts) x :) 
                          (code x :))
                      __
                      no-type)))
   'next
   (blank)
   'alts~
   (type-tree _ (hbl-append (open-paren) (code lambda {[x : bool]}) __) (code x) (close-paren) _ #f #f _ _
              (code _bool) #f #f (-> (code _bool) (code _bool)) #f vr-append
              (inenv (tbind (code x) (code _bool))) #f #f (mtenv) #f #f)
   'next
   (blank)
   'alts~
   ((if rules
        shrinker
        (lambda (x) (x)))
    (lambda ()
      (let ([xbind (inenv (tbind (code x) (code _bool)))])
        (type-tree (hbl-append (open-paren) (code lambda {[x : bool]}) __) 
                   (hbl-append (open-paren) (code if) __) (code x) __ (code 1) __ (code 2) (close-paren) (close-paren)
                   (code _bool) (code _num) (code _num) (code _num) (-> (code _bool) (code _num)) vr-append
                   xbind xbind xbind xbind (mtenv) #t))))))

(variable-types type-tree #f)
(variable-types type-tree/formal (vc-append (current-font-size)
                                            (var-rule)
                                            (func-rule)))

(slide
 #:title "Revised Rules"
 (num-rule)
 (code #,(E) #,(ts) #t : _bool)
 (code #,(E) #,(ts) #f : _bool)
 (blank)
 (plus-rule)
 (blank)
 (infer (code #,(E) #,(ts) {if #,(M 1) #,(M 2) #,(M 3)} : #,(T 0))
        (ante-append
         (code #,(E) #,(ts) #,(M 1) : _bool)
         (code #,(E) #,(ts) #,(M 2) : #,(T 0))
         (code #,(E) #,(ts) #,(M 3) : #,(T 0)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 6)

(define (function-call-types type-tree rules)
  (slide
   #:title (if rules
               "Function Call Type Rule"
               "Types: Function Calls")
   (or rules 'nothing)
   (if rules 'next 'nothing)
   (if rules (blank) 'nothing)
   'alts~
   ((if rules
        shrinker
        (lambda (x) (x)))
    (lambda ()
      (type-tree _ (open-paren) (code {lambda {[x : bool]} {if x 1 2}}) __ (code #t) #f #f (close-paren) _
                 (-> (code _bool) (code _num)) (code _bool) #f (code _num) #f vc-append
                 (mtenv) (mtenv) #f (mtenv) #f #f)))
   'next
   (blank)
   'alts~
   ((if rules
        shrinker
        (lambda (x) (x)))
    (lambda ()
      (type-tree _ (open-paren) (code {lambda {[x : bool]} {if x 1 2}}) __ (code 5) #f #f (close-paren) _
                 (-> (code _bool) (code _num)) (code _num) #f no-type #f vc-append
                 (mtenv) (mtenv) #f (mtenv) #f #f)))
   'next
   (blank)
   'alts~
   (type-tree _ (open-paren) (code 7) __ (code 5) #f #f (close-paren) _
              (code _num) (code _num) #f no-type #f vc-append
              (mtenv) (mtenv) #f (mtenv) #f #f)))

(function-call-types type-tree #f)
(function-call-types type-tree/formal (apply-rule))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 7)

(slide
 #:title "Types: Multiple Arguments"
 'alts~
 (type-tree (hbl-append (open-paren) (code lambda {[x : num] [y : num]}) __)
            (hbl-append (open-paren) (code +) __) (code x) __ (code y) #f #f (close-paren) (close-paren)
            (code _num) (code _num) #f (code _num) (-> (arg-crs (code _num) (code _num)) (code _num)) vr-append)
 'next
 (blank)
 'alts~
 (type-tree _ (open-paren) (code {lambda {[x : num] [y : num]} {+ x y}})
            __ (code 5) __ (code 6) (close-paren) _
            (-> (arg-crs (code _num) (code _num)) (code _num)) (code _num) (code _num) (code _num) #f vc-append)
 'next
 (blank)
 'alts~
 (type-tree _ (open-paren) (code {lambda {[x : num] [y : num]} {+ x y}})
            __ (code 5) #f #f (close-paren) _
            (-> (arg-crs (code _num) (code _num)) (code _num)) (code _num) #f no-type #f vc-append))

(define func-rule/multi
  (scale/improve-new-text
   (infer (hbl-append
           (code #,(E) #,(ts) {lambda {[#,(subnt "Symbol" 1) : #,(T 1)] #,(t "...") [#,(subnt "Symbol" "n") : #,(T "n")]} #,(M)})
           __ (code :) __ (-> (arg-crs (T 1) (hbl-append (t "... ") (T "n"))) (T 0)))
          (hbl-append
           (E+ (E) (inenv (hbl-append (tbind (subnt "Symbol" 1) (T 1)) (t " ... ") (tbind (subnt "Symbol" "n") (T "n")))))
           __ (code #,(ts) #,(M) : #,(T 0))))
   0.8))

(define apply-rule/multi
  (scale/improve-new-text
   (infer (code #,(E) #,(ts) {#,(M 0) #,(M 1) #,(t "...") #,(M "n")} : #,(T 0))
          (ante-append 
           (code #,(E) #,(ts) #,(M 0) : #,(-> (arg-crs (T 1) (hbl-append (t "... ") (T "n"))) (T 0)))
           (code #,(E) #,(ts) #,(M 1) : #,(T 1))
           (t "...")
           (code #,(E) #,(ts) #,(M "n") : #,(T "n"))))
   0.8))

(slide
 #:title "Revised Function and Call Rules"
 func-rule/multi
 (blank)
 (blank)
 apply-rule/multi)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 8)

(slide
 #:title "Typed Language"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)           -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)           -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)           -or- (code #,(nonterm "Symbol")) (blank)
        (blank)           -or- (code {lambda {[#,(nonterm "Symbol") : #,(nonterm "Type")]} #,(nonterm "Exp")}) (blank)
        (blank)           -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (ghost (nonterm "Type")) (blank) (blank) (blank)
        (nonterm "Type")    eqls (code num) (blank)
        (blank)           -or- (code bool) (blank)
        (blank)           -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Expressions"
 (code
  (define-type Exp
    (numE [n : Number])
    (idE [s : Symbol])
    (plusE [l : Exp] 
           [r : Exp])
    (multE [l : Exp]
           [r : Exp])
    (lamE [n : Symbol]
          [arg-type : Type]
          [body : Exp])
    (appE [fun : Exp]
          [arg : Exp]))))

(slide
 #:title "Types and Type Bindings"
 (scale/improve-new-text
  (code
   (define-type Type
     (numT)
     (boolT)
     (arrowT [arg : Type]
             [result : Type]))
   code:blank
   (define-type Type-Binding
     (tbind [name : Symbol]
            [type : Type]))
   code:blank
   (define-type-alias Type-Env (Listof Type-Binding)))
  0.9))

(define (tc-impl* part)
  (code
   (define typecheck : (Exp Type-Env -> Type)
     (lambda (a tenv)
       (type-case Exp a
         ...
         #,part
         ...)))))

(define-syntax tc-impl
  (syntax-rules ()
    [(_ x r) (list 
              (cb-superimpose
               (lt-superimpose
                (get-titleless-page #:aspect 'fullscreen)
                (para #:width (get-client-w #:aspect 'fullscreen)
                      (blank) 
                      (scale/improve-new-text (tc-impl* x) 0.8)))
               (vc-append gap-size r (blank))))]))

(slide
 #:title "Type Checker"
 'alts
 (list
  (tc-impl (code ...) (blank))
  (tc-impl (code [(numE n) ...])
           (num-rule))
  (tc-impl (code [(numE n) (numT)])
           (num-rule))
  (tc-impl (code [(plusE l r) 
                  ...])
           
           (plus-rule))
  (tc-impl (code [(plusE l r) 
                  ... (typecheck l tenv) ...
                  ... (typecheck r tenv) ...])
           
           (plus-rule))
  (tc-impl (code [(plusE l r) 
                  (type-case Type (typecheck l tenv)
                    [(numT)
                     ... (typecheck r tenv) ...]
                    [else (type-error l "num")])])
           (plus-rule))
  (tc-impl (code [(plusE l r) 
                  (type-case Type (typecheck l tenv)
                    [(numT)
                     (type-case Type (typecheck r tenv)
                       [(numT) (numT)]
                       [else (type-error r "num")])]
                    [else (type-error l "num")])])
           (plus-rule))
  (tc-impl (code [(idE name) ...])
           (var-rule))
  (tc-impl (code [(idE name) (type-lookup name tenv)])
           (var-rule))
  (tc-impl (code [(lamE n arg-type body)
                       ...])
           (func-rule))
  (tc-impl (code [(lamE n arg-type body)
                  ... (typecheck body ...) ...])
           (func-rule))
  (tc-impl (code [(lamE n arg-type body)
                  ... (typecheck body (extend-env
                                       (tbind n arg-type)
                                       tenv)) ...])
           (func-rule))
  (tc-impl (code [(lamE n arg-type body)
                  (arrowT arg-type
                          (typecheck body (extend-env
                                           (tbind n arg-type)
                                           tenv)))])
           (func-rule))
  (tc-impl (code [(appE fun arg)
                  ...])
           (apply-rule))
  (tc-impl (code [(appE fun arg)
                  ... (typecheck fun tenv) ...
                  ... (typecheck arg tenv) ...])
           (apply-rule))
  (tc-impl (code [(appE fun arg)
                  (type-case Type (typecheck fun tenv)
                    [(arrowT arg-type result-type)
                     ... (typecheck arg tenv) ...]
                    [else (type-error fun "function")])])
           (apply-rule))
  (tc-impl (code [(appE fun arg)
                  (type-case Type (typecheck fun tenv)
                    [(arrowT arg-type result-type)
                     (if (equal? arg-type
                                 (typecheck arg tenv))
                         result-type
                         (type-error arg
                                     (to-string arg-type)))]
                    [else (type-error fun "function")])])
           (apply-rule))))

;; ----------------------------------------
(part 9)

(slide
 #:title (hbl-append (code typecheck) (titlet " and ") (code interp))
 (para #:aspect 'fullscreen
       "Only call" (code interp) "on an expression for which" (code typecheck) "produces a type")
 'next
 (blank)
 (para #:aspect 'fullscreen
       (code typecheck) (bit "never") "calls" (code interp))
 'next
 (blank)
 (para #:aspect 'fullscreen
       (code interp) (bit "never") "calls" (code typecheck)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 10)

(define (mk-pair-ex t1 t2 t3 v1 v2)
  (scale
   (code
    {let {[pair : (#,t1 -> (#,t2 -> (bool -> #,t3)))
                {lambda {x : #,t1}
                  {lambda {y : #,t2}
                    {lambda {s : bool}
                      {if s x y}}}}]}
      {let {[fst : ((bool -> #,t3) -> #,t3)
                   {lambda {p : (bool -> #,t3)}
                     {p #t}}]}
        {let {[snd : ((bool -> #,t3) -> #,t3)
                    {lambda {p : (bool -> #,t3)}
                      {p #f}}]}
          {snd {{pair #,v1} #,v2}}}}})
   0.85))

(define pairs-title "Pairs")

(define dots (colorize (t "...") RedColor))

(slide
 #:title pairs-title
 'alts
 (list
  (list
   (let ([n (code num)])
     (mk-pair-ex n n n (code 1) (code 2))))
  (list
   (let ([n (code bool)])
     (mk-pair-ex n n n (code #t) (code #f))))
  (list
   (mk-pair-ex (code num) (code bool) (code #,dots) (code 1) (code #f))
   'next
   (blank)
   (para #:align 'center (bt "No possible type for") dots))))

(slide
 #:title "Language with Pairs"
 #:layout 'tall
 (scale/improve-new-text
  (grammar-table
   (list (nonterm "Exp") eqls (nonterm "Number") (blank)
         (blank)           -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code #,(nonterm "Symbol")) (blank)
         (blank)           -or- (code {lambda {[#,(nonterm "Symbol") : #,(nonterm "TE")]} #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {pair #,(nonterm "Exp") #,(nonterm "Exp")}) new-label
         (blank)           -or- (code {fst #,(nonterm "Exp")}) new-label
         (blank)           -or- (code {snd #,(nonterm "Exp")}) new-label
         (ghost (nonterm "Type")) (blank) (blank) (blank)
         (nonterm "Type") eqls (code num) (blank)
         (blank)           -or- (code bool) (blank)
         (blank)           -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank)
         (blank)           -or- (code (#,(nonterm "Type") * #,(nonterm "Type"))) new-label))
  0.8)
 'next
 (blank 0)
 'alts
 (list (list
        (infer (code #,(E) #,(ts) {pair #,(M 1) #,(M 2)} : #,(crs (T 1) (T 2)))
               (ante-append
                (code #,(E) #,(ts) #,(M 1) : #,(T 1))
                (code #,(E) #,(ts) #,(M 2) : #,(T 2)))))
       (list
        (infer (code #,(E) #,(ts) {fst #,(M)} : #,(T 1))
               (code #,(E) #,(ts) #,(M) : #,(crs (T 1) (T 2)))))
       (list
        (infer (code #,(E) #,(ts) {snd #,(M)} : #,(T 2))
               (code #,(E) #,(ts) #,(M) : #,(crs (T 1) (T 2)))))))


