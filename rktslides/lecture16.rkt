#lang slideshow

(require slideshow/code
         slideshow/balloon
         racket/draw
         "utils/colors.ss"
         "utils/utils.ss"
         "utils/alg.ss"
         "utils/types.ss"
         "utils/fae-types.ss"
         "utils/tvfae-types.ss"
         (lib "list.ss"))

(current-keyword-list
 (cons "let-type" (current-keyword-list)))

(define WIDE (* (get-client-w #:aspect 'fullscreen) 0.9))

(define (align p)
  (para #:width WIDE p))

(define (xalign p)
  (align (scale p 1.0)))

(set-spotlight-style! #:color (let ([c (send the-color-database find-color "pink")])
                                (make-color (send c red)
                                            (send c green)
                                            (send c blue)
                                            0.7)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 1)

(slide
 #:title "Recursion"
 (sscode
  {let {[mk-rec {lambda {body}
                  {{lambda {fX} {fX fX}}
                   {lambda {fX}
                     {{lambda {f} {body f}}
                      {lambda {x} {{fX fX} x}}}}}}]}
    {let {[fib {mk-rec 
                {lambda {fib}
                  {lambda {n}
                    {if0 n
                         1
                         {if0 {- n 1}
                              1
                              {+ {fib {- n 1}} 
                                 {fib {- n 2}}}}}}}}]}
      {fib 4}}}))
    
(define dots (colorize (t "...") RedColor))

(slide
 #:title "Typed Recursion"
 (sscode
  {let {[mk-rec : (((num -> num) -> (num -> num)) -> (num -> num))
                {lambda {[body : ((num -> num) -> (num -> num))]}
                  {{lambda {[fX : (#,dots -> (num -> num))]} {fX fX}}
                   {lambda {[fX : (#,dots -> (num -> num))]}
                     {{lambda {[f : (num -> num)]} {body f}}
                      {lambda {[x : num]} {{fX fX} x}}}}}}]}
    {let {[fib : (num -> num)
               {mk-rec 
                {lambda {[fib : (num -> num)]}
                     {lambda {[n : num]}
                          {if0 n
                               1
                               {if0 {- n 1}
                                    1
                                    {+ {fib {- n 1}} 
                                       {fib {- n 2}}}}}}}}]}
      {fib 4}}})
 'next
 (blank)
 'alts
 (list
  (list
   (para #:align 'center (bt "Nothing works in place of") dots))
  (list
   (para #:align 'center (bt "Theorem:") "programs in the" (it "simply typed λ-calculus") "always terminate"))))

(slide
 #:title "Extending the Type System"
 (para #:width WIDE "When encodings fail,"
       "extend the language and type system")
 'next
 (blank)
 (blank)
 (para #:width WIDE "In this case, add" (code letrec) "as a core form, again")
 (sscode
  {letrec {[fib : (num -> num)
                {lambda {[n : num]}
                  {if0 n
                       1
                       {if0 {- n 1}
                            1
                            {+ {fib {- n 1}} 
                               {fib {- n 2}}}}}}]}
    {fib 4}}))

(slide
 #:title "Grammar with Recursion"
 (scale/improve-new-text
  (grammar-table
   (list (nonterm "Exp") eqls (nonterm "Number") (blank)
         (blank)           -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code #,(nonterm "Symbol")) (blank)
         (blank)           -or- (code {lambda {[#,(nonterm "Symbol") : #,(nonterm "Type")]} #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {letrec {[#,(nonterm "Symbol") : #,(nonterm "Type") #,(nonterm "Exp")]} #,(nonterm "Exp")})
         new-label
         (blank) (blank) (tt " ") (blank)
         (nonterm "Type") eqls (code num) (blank)
         (blank)           -or- (code bool) (blank)
         (blank)           -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank)))
  0.8))

(slide
 #:title "Datatypes"
 (code
  (define-type Exp
    ...
    (letrecE [name : Symbol]
             [rhs-type : Type]
             [rhs : Exp]
             [body : Exp]))
  code:blank
  code:blank
  (define-type Binding
    (bind [name : Symbol]
          [val : (Boxof (Optionof Value))]))))


(slide
 #:title "Interpreter with Recursion"
 (scode
  (define (interp a env)
    (type-case Exp env
      ...
      [(letrecE n t rhs body)
       (let ([b (box (none))])
         (let ([new-env (extend-env
                         (bind n b)
                         env)])
           (begin
             (set-box! b (some (interp rhs new-env)))
             (interp body new-env))))]))))

(define rec-rule
  (infer (code #,(E) #,(ts) {letrec {[#,(N) : #,(T 0) #,(M 0)]} #,(M 1)} : #,(T 1))
         (let ([big-E (E+ (E) (inenv (tbind (N) (T 0))))])
           (ante-append
            (code #,big-E #,(ts) #,(M 0) : #,(T 0))
            (code #,big-E #,(ts) #,(M 1) : #,(T 1))))))

(slide
 #:title "Type Checking Recursion"
 rec-rule
 (blank)
 (blank)
 (blank)
 (htl-append (t "Abbreviation: ")
             __
             (para #:fill? #f (N) " = " (nonterm "Symbol"))))

(define (rec-case/fun body)
  (scale
   (code
    (define typecheck : (Exp Type-Env -> Type)
      (lambda (a env)
        (type-case Exp a
          ...
          [(letrecE n rhs-type rhs body)
           #,body]))))
   0.8))

(define-syntax-rule (rec-case e ...)
  (rec-case/fun (code e ...)))

(let ([all
       (list
        (rec-case ....)
        (rec-case ....
                  (typecheck rhs ....)
                  (typecheck body ....)
                  ....)
        (rec-case (let ([new-tenv (extend-env
                                   (tbind n rhs-type)
                                   tenv)])
                    ....
                    (typecheck rhs new-tenv)
                    (typecheck body new-tenv)
                    ....))
        (rec-case (let ([new-tenv (extend-env
                                   (tbind n rhs-type)
                                   tenv)])
                    (if (equal? rhs-type
                                (typecheck rhs new-tenv))
                        (typecheck body new-tenv)
                        (type-error rhs (to-string rhs-type))))))])
  (for ([e (in-list all)])
    (slide
     #:title "Type Checker with Recursion"
     (lt-superimpose (ghost (apply lt-superimpose all))
                     e)
     (blank)
     (blank)
     rec-rule)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 2)

(define variants-title "Variants")

(slide
 #:title "Types of Data"
 (item "Atomic")
 (vc-append
  (current-line-sep)
  (subitem "numbers")
  (subitem "booleans")
  (subitem "..."))
 'next
 (blank)
 (item "Compound")
 (vc-append
  (current-line-sep)
  (subitem "Tuples")
  (subitem "Records")
  (subitem "..."))
 'next
 (blank)
 (item "Variants"))

(slide
 #:title variants-title
 (code
  (define-type Grade
    (percent [r : Number])
    (pass/fail [pass? : Boolean]))
  code:blank
  (define-type Chain
    (end [r : Number])
    (link [next : Chain]))
  code:blank
  (define-type IntList
    (iempty [d : Number])
    (icons [p : (Number * IntList)]))
  code:blank
  ...))

#;
(slide
 #:title variants-title
 (sscode
  {let {[left : (num -> (num * num))
              {lambda {[x : num]}
                {pair 0 x}}]}
    {let {[right : (num -> (num * num))
                 {lambda {[x : num]}
                   {pair 1 x}}]}
      {let {[displacement : ((num * num) -> num)
                          {lambda {[p : (num * num)]}
                            {if0 {fst p}
                                 {- 0 {snd p}}
                                 {snd p}}}]}
        {displacement {left 5}}}}}))

(slide
 #:title variants-title
 (sscode
  {let {[percent : (num -> (num * (num * bool)))
                 {lambda {[x : num]}
                   {pair 0 {pair x #f}}}]}
    {let {[passfail : (num -> (num * (num * bool)))
                    {lambda {[y : bool]}
                      {pair 1 {pair 0 y}}}]}
      {let {[pass? : ((num * (num * bool)) -> bool)
                   {lambda {[p : (num * (num * bool))]}
                     {if0 {fst p}
                          {> {fst {snd p}} 70}
                          {snd {snd p}}}}]}
        {pass? {percent 96}}}}})
 (blank)
 'next
 (para
  #:align 'right
  (para #:fill? #f
        "Have to make up a value for the other type,"
        "but this can be made to work always using thunks")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rec-title "Recursive Datatypes")

(define xdots (inset (colorize (t "...") RedColor) 0 -3 0 0))

(define (chain #:d4 [d4 xdots]
               #:d3 [d3 xdots]
               #:d2 [d2 d3]
               #:d1 [d1 d2])
  (list
   (align
    (scale
     (code
      {let {[end : (num -> #,d1)
                 {lambda {[d : num]}
                   {pair 0 d}}]}
        {let {[link : (#,d2 -> #,d4)
                    {lambda {[r : #,d3]}
                      {pair 1 r}}]}
          {link {link {link {end 0}}}}}})
     0.85))))

(slide
 #:title rec-title
 'alts
 (list
  (chain)
  (chain #:d1 (code (num * num)))
  (chain #:d2 (code (num * num)))
  (chain #:d3 (code (num * num)))
  (append
   (chain #:d3 (code (num * num)) #:d4 (code (num * (num * num))))
   (list
    'next
    (blank)
    (blank)
    (para #:align 'center (bt "Stuck") "...")))))

(slide
 #:title rec-title
 (scode
  {let {[iempty : (num -> (num * #,dots))
                {lambda {[d : num]}
                  {pair 0 d}}]}
    {let {[icons : (num -> ((num * #,dots) -> (num * #,dots)))
                 {lambda {[x : num]}
                   {lambda {[r : (num * #,dots)]}
                     {pair 1 {pair x r}}}}]}
      {{icons 1} {{icons 2} {{icons 3} {iempty 0}}}}}})
 (blank)
 (blank)
 (para #:align 'center (bt "Stuck again with") dots))

(slide
 #:title rec-title
 (para #:width (* client-w 0.9) "Add" (code let-type) "and" (code type-case) ":")
 (blank)
 (xscode
  {let-type [Numlist {iempty num}
                     {icons (num * Numlist)}]
    {letrec {[len : (Numlist -> num)
                  {lambda {[l : Numlist]}
                    {type-case Numlist l
                      [{iempty n} 0]
                      [{icons fxr} {+ 1 {len {snd fxr}}}]}}]}
      {len {icons {pair 1 {icons {pair 2 {iempty 0}}}}}}}}))

(slide
 #:title "Grammar with Variants"
 (scale/improve-new-text
  (grammar-table
   (list (nonterm "Exp") eqls (nonterm "Number") (blank)
         (blank)           -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {- #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code #,(nonterm "Symbol")) (blank)
         (blank)           -or- (code {lambda {[#,(nonterm "Symbol") : #,(nonterm "Type")]} #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {letrec {[#,(nonterm "Symbol") : #,(nonterm "Type") #,(nonterm "Exp")]} #,(nonterm "Exp")})
         (blank)
         (blank)           -or- (code {let-type [#,(nonterm "Symbol") {#,(nonterm "Symbol") #,(nonterm "Type")}
                                                 #||#     {#,(nonterm "Symbol") #,(nonterm "Type")}]
                                        #,(nonterm "Exp")})
         new-label
         (blank)           -or- (code {type-case #,(nonterm "Symbol") #,(nonterm "Exp")
                                        [{#,(nonterm "Symbol") #,(nonterm "Symbol")} #,(nonterm "Exp")]
                                        [{#,(nonterm "Symbol") #,(nonterm "Symbol")} #,(nonterm "Exp")]}) 
         new-label
         (ghost (nonterm "TE")) (blank) (blank) (blank)
         (nonterm "Type") eqls (code num) (blank)
         (blank)           -or- (code bool) (blank)
         (blank)           -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank)
         (blank)           -or- (nonterm "Symbol") new-label))
  0.8))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 3)

(define (Fruit/proc b)
  (code
   (interp (parse `{let-type [Fruit {apple num}
                                    {banana (num -> num)}]
                     #,(colorize b (current-literal-color))})
           mt-env)))

(define-syntax-rule (Fruit b)
  (Fruit/proc (parameterize ([code-colorize-enabled #f])
                (code b))))

(slide
 #:title "Examples"
 'alts
 (list
  (list
   (xalign
    (code
     (test #,(Fruit ...)
           ...))))
  (list
   (xalign
    (code
     (test #,(Fruit 1)
           (numV 1)))))
  (list
   (xalign
    (code
     (test #,(Fruit {apple 1})
           ....))))
  (list
   (xalign
    (code
     (test #,(Fruit {apple 1})
           (appleV (numV 1))) (code:comment "???"))))
  (list
   (xalign
    (code
     (test #,(Fruit {apple 1})
           (variantV 'apple (numV 1))))))
  (list
   (xalign
    (code
     (test #,(Fruit {banana {lambda {[x : num]} 5}})
           (variantV 'banana (closV ....))))))
  (list
   (xalign
    (code
     (test #,(Fruit apple)
           ....))))
  (list
   (xalign
    (code
     (test #,(Fruit apple)
           (constructorV 'apple)))))
  (list
   (xalign
    (code
     (test #,(Fruit banana)
           (constructorV 'banana)))))
  (list
   (xalign
    (code
     (test #,(Fruit {type-case Fruit {apple 3}
                      [{apple a} a]
                      [{banana b} {b 7}]})
           (numV 3)))))
  (list
   (xalign
    (code
     (test #,(Fruit {type-case Fruit {banana
                                      {lambda {[x : num]}
                                        {+ x 3}}}
                      [{apple a} a]
                      [{banana b} {b 7}]})
           (numV 10)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 4)

(start-at-recent-slide)

(slide
 #:title "Value Datatype"
 (code
  (define-type Value
    ....
    (variantV [tag : Symbol]
              [val : Value])
    (constructorV [tag : Symbol])))
 'next
 (blank (* 2 gap-size))
 'alts
 (list
  (list
   (code (constructorV 'apple)))
  (list
   (vl-append
    gap-size
    (para #:fill? #f "Apply" (code (constructorV 'apple)) "to" (code (numV 1)))
    (para #:fill? #f "⇒" (code (variantV 'apple (numV 1))))))))

(define let-type-schema
  (code {let-type [#,(nonterm "Symbol") {#,(nonterm "Symbol") #,(nonterm "Type")}
                   #||#     {#,(nonterm "Symbol") #,(nonterm "Type")}]
          #,(nonterm "Exp")}))

(define type-case-schema
  (code {type-case #,(nonterm "Symbol") #,(nonterm "Exp")
          [{#,(nonterm "Symbol") #,(nonterm "Symbol")} #,(nonterm "Exp")]
          [{#,(nonterm "Symbol") #,(nonterm "Symbol")} #,(nonterm "Exp")]}))

(define (background p)
  (cc-superimpose (colorize
                   (filled-rectangle (+ (pict-width p) 5)
                                     (+ (pict-height p) 5))
                   balloon-color)
                  p))

(define (zero show? p)
  (let ([p (scale p 0.7)])
    (define b (blank))
    (refocus (lt-superimpose b
                             ((if show? values ghost)
                              (background p)))
             b)))

(define (exp-slides #:let-type? [let-type? #f]
                    #:type-case? [type-case? #f])
  (slide
   #:title "Expressions for Variants"
   (sscode
    (define-type Exp
      ....
      (let-typeE [type-name : Symbol]  #,(zero let-type? let-type-schema)
                 [tag1 : Symbol]
                 [type1 : Type]
                 [tag2 : Symbol]
                 [type2 : Type]
                 [body : Exp])
      (type-caseE [type-name : Symbol] #,(zero type-case? type-case-schema)
                  [tst : Exp]
                  [tag1 : Symbol]
                  [n1 : Symbol]
                  [rhs1 : Exp]
                  [tag2 : Symbol]
                  [n2 : Symbol]
                  [rhs2 : Exp]))
    code:blank
    (define-type Type
      ....
      (definedT [name : Symbol])))))

(exp-slides)
(exp-slides #:let-type? #t)
(exp-slides #:type-case? #t)
(exp-slides)

(define (add-example e p)
  (list
   'alts
   (list
    (list p)
    (list
     (refocus
      (rt-superimpose
       (ct-superimpose p (blank (* client-w 0.8)))
       (background (scale e 0.75)))
      p)))))

(slide
 #:title "Interpreter"
 'alts
 (list
  (add-example
   (code {let-type [Fruit {apple num}
                          {banana (num -> num)}]
           {apple 10}})
   (align
    (sscode
     (define (interp a tenv)
       (type-case Exp a
         ....
         [(let-typeE type-name
                     tag1 type1
                     tag2 type2
                     body)
          (interp body
                  (extend-env
                   (bind tag1
                         (constructorV tag1))
                   (extend-env
                    (bind tag2
                          (constructorV tag2))
                    env)))]
         ....)))))
  (add-example
   (code {let-type [Fruit {apple num}
                          {banana (num -> num)}]
           {apple 10}})
   (align
    (sscode
     (define (interp a tenv)
       (type-case Exp a
         ....
         [(appE fun arg)
          (type-case Value (interp fun env)
            [(closV n body c-env) ....]
            [(constructorV tag)
             (variantV tag (interp arg env))]
            [else (error 'interp "not a function")])]
         ....)))))
  (add-example
   (code {type-case Fruit {apple 10}
           [{apple a} a]
           [{banana b} {b 7}]})
   (align
    (sscode
     (define (interp a tenv)
       (type-case Exp a
         ....
         [(type-caseE type-name tst
                      tag1 n1 rhs1
                      tag2 n2 rhs2)
          (type-case Value (interp tst env)
            [(variantV tag val)
             (cond
               [(eq? tag tag1) (interp rhs1
                                       (extend-env
                                        (bind n1 (box (some val)))
                                        env))]
               [(eq? tag tag2) (interp rhs2
                                       (extend-env
                                        (bind n2 (box (some val)))
                                        env))]
               [else (error 'interp "wrong tag")])]
            [else (error 'interp "not a variant")])]
         ....)))))))

(define (tFruit/proc b)
  (code
   (typecheck
    (parse `{let-type [Fruit {apple num}
                             {banana (num -> num)}]
              #,(colorize b (current-literal-color))})
    mt-env)))

(define-syntax-rule (tFruit b)
  (tFruit/proc (parameterize ([code-colorize-enabled #f])
                 (code b))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 5)

(slide
 #:title "Typechecking Examples"
 'alts
 (list
  (list
   (xalign
    (code
     (test #,(tFruit ...)
           ...))))
  (list
   (xalign
    (code
     (test #,(tFruit 1)
           (numT)))))
  (list
   (xalign
    (code
     (test #,(tFruit {apple 1})
           (definedT 'Fruit)))))
  (list
   (xalign
    (code
     (test #,(tFruit {banana {lambda {[x : num]} x}})
           (definedT 'Fruit)))))
  (list
   (xalign
    (code
     (test #,(tFruit
              {type-case Fruit {apple 3}
                [{apple a} a]
                [{banana b} {b 7}]})
           (numT))))
   'next
   (blank)
   (blank)
   (para #:width WIDE (code type-case) "results must all have the same type"))
  (list
   (xalign
    (code
     (test/exn #,(tFruit
                  {type-case Fruit {apple 3}
                    [{radish a} a]
                    [{banana b} {b 7}]})
               "no type")))
   'next
   (blank)
   (blank)
   (para #:width WIDE (code typecheck) "must record variant names for" (code let-type)))
  (list
   (xalign
    (code
     (test/exn #,(tFruit
                  {type-case Fruit {apple 3}
                    [{apple a} a]
                    [{banana b} b]})
               "no type")))
   'next
   (blank)
   (blank)
   (para #:width WIDE (code typecheck) "must record variant types for" (code let-type)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 6)

(slide
 #:title "Bindings for Defined Types"
 'alts
 (let ([expr
        (code {let-type [#,(Q) {#,(N 1) #,(T 1)}
                         #||#  {#,(N 2) #,(T 2)}]
                ...})]
       [bind (hbl-append
              (E)
              (inenv
               (hbl-append (Q) (t " = ") 
                           (cbind (N 1) (T 1)) 
                           (t "+")
                           (cbind (N 2) (T 2)))))]
       [decl (code
              (define-type Type-Binding
                ....
                (tdef [type-name : Symbol]
                      [tag1 : Symbol]
                      [type1 : Type]
                      [tag2 : Symbol]
                      [type2 : Type])))]
       [spacer (blank gap-size)])
   (list
    (list expr
          spacer
          bind)
    (list bind
          spacer
          decl))))

(slide
 #:title "Local Type Names"
 (item #:width (get-client-w #:aspect 'fullscreen) "Might be ok:")
 (code
  {let-type [Fruit {apple num}
                   {banana (num -> num)}]
            ... {lambda {[x : Fruit]} ...} ...})
 'next
 (item #:width (get-client-w #:aspect 'fullscreen)  "Not ok:")
 (code
  {lambda {[x : Fruit]} ...})
 'next
 (blank)
 (blank)
 'alts
 (let* ([num-rule (code #,(E) #,(ts) num)]
        [ex-ts (ts)]
        [p (vc-append
            gap-size
            (let* ([binding (hbl-append (Q) (t " = ") 
                                        (cbind (N 1) (T 1)) 
                                        (t "+")
                                        (cbind (N 2) (T 2)))]
                   [env (inenv (code #,(t "...") #,binding #,(t "...")))])
              (code #,env #,ex-ts #,(Q)))
            (blank)
            (hc-append (* 4 gap-size)
                       num-rule
                       (code #,(E) #,(ts) bool)
                       (infer (code #,(E) #,(ts) (#,(T 1) -> #,(T 2)))
                              (ante-append
                               (code #,(E) #,(ts) #,(T 1))
                               (code #,(E) #,(ts) #,(T 2))))))])
   (list
    (list p)
    (list (pin-balloon (wrap-balloon (code tvarcheck)
                                     's 0 gap-size)
                       p
                       ex-ts ct-find)))))

;; ----------------------------------------
(part 7)

(slide
 #:title "Type Checking"
 (func-rule #:tvarcheck? #t)
 'next
 (blank)
 (blank)
 (blank)
 (xscode
  (define (typecheck a tenv)
    (type-case Exp a
      ....
      [(lamE n arg-type body)
       (begin
         (tvarcheck arg-type tenv)
         (arrowT arg-type
                 (typecheck body 
                            (extend-env (tbind n arg-type)
                                        tenv))))]
      ....))))

(slide
 #:title "Type Checker with Variants"
 (blank)
 (withtype-rule (lambda () null))
  'next
 (blank)
 (blank)
 (blank)
 (cases-rule (lambda () null))
 'next
 (blank)
 (blank)
 (blank)
 (para 
  #:align 'right
  (frame
   (scale/improve-new-text
    (inset
     (para #:width (* (get-client-w #:aspect 'fullscreen) 3/4)
           #:fill? #f
           (bt "Warning:") "this"
           (code let-type) "rule is not quite right...")
     gap-size)
    0.75))))

(slide
 #:title "Type Checking"
 #:layout 'tall
 'alts
 (list
  (add-example
   (code {let-type [Fruit {apple num}
                          {banana (num -> num)}]
           {apple 10}})
   (scale/improve-new-text
    (sscode
     (define (typecheck a tenv)
       (type-case Exp a
         ....
         [(let-typeE type-name
                     tag1 type1 
                     tag2 type2 
                     body)
          (let ([new-tenv (extend-env (tdef type-name
                                            tag1 type1
                                            tag2 type2)
                                      tenv)])
            (begin
              (tvarcheck type1 new-tenv)
              (tvarcheck type2 new-tenv)
              (typecheck body (extend-env
                               (tbind tag1
                                      (arrowT type1 (definedT type-name)))
                               (extend-env
                                (tbind tag2
                                       (arrowT type2 (definedT type-name)))
                                new-tenv)))))]
         ....)))
    1.0))
  (add-example
   (code {type-case Fruit {apple 10}
           [{apple a} a]
           [{banana b} {b 7}]})
   (scale/improve-new-text
    (sscode
     (define (typecheck a tenv)
       (type-case Exp a
         ....
         [(type-caseE type-name tst
                      tag1 n1 rhs1
                      tag2 n2 rhs2)
          (let ([def (defined-type-lookup type-name tenv)])
            ....
            (type-case Type (typecheck tst tenv)
              [(definedT name) 
                .... (equal? name type-name)
                .... (typecheck rhs1 (extend-env
                                      (tbind n1 (tdef-type1 def))
                                      tenv))
                .... (typecheck rhs2 (extend-env
                                      (tbind n2 (tdef-type2 def))
                                      tenv))
                ....])
            ....)]
         ....)))
    1.0))
  (list
   (scale/improve-new-text
    (sscode
     (define (typecheck a tenv)
       (type-case Exp a
         ....
         [(type-caseE type-name tst
                      tag1 n1 rhs1
                      tag2 n2 rhs2)
          (let ([def (defined-type-lookup type-name tenv)])
            (if (not (and (equal? tag1 (tdef-tag1 def))
                          (equal? tag2 (tdef-tag2 def))))
                (type-error a "matching variant names")
                (type-case Type (typecheck tst tenv)
                  [(definedT name) 
                    (if (not (equal? name type-name))
                        (type-error tst (to-string type-name))
                        (let ([rhs1-t (typecheck rhs1 (extend-env
                                                       (tbind n1 (tdef-type1 def))
                                                       tenv))]
                              [rhs2-t  (typecheck rhs2 (extend-env
                                                        (tbind n2 (tdef-type2 def))
                                                        tenv))])
                          (if (equal? rhs1-t rhs2-t)
                              rhs1-t
                              (type-error rhs2 (to-string rhs1-t)))))]
                  [else (type-error tst (to-string type-name))])))]
         ....)))
    0.9))))

(slide
 #:title "Checking Type Forms"
 (code
  (define (tvarcheck ty tenv)
    (type-case Type ty
      [(numT) (values)]
      [(boolT) (values)]
      [(arrowT a b) (begin
                      (tvarcheck a tenv)
                      (tvarcheck b tenv))]
      [(definedT id) (begin
                       (defined-type-lookup id tenv)
                       (values))]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 8)

(define (sound-para . l)
  (apply para #:width (* 0.6 (get-client-w #:aspect 'fullscreen)) l))

(slide
 #:title "Type Soundness"
 (para (dt "Type soundness") "is a theorem of the form")
 (sound-para "If" (code #,(mtenv) #,(ts) #,(M) : #,(T)) ", then"
             "running" (M) "never produces an error")
 'next
 (blank)
 (blank)
 (para "If we add division, then divide-by-zero errors may be ok:")
 (sound-para "If" (code #,(mtenv) #,(ts) #,(M) : #,(T)) ", then"
             "running" (M) "never produces an error except divide-by-zero")
 (blank)
 (para "In general, soundness rules out a certain class of run-time errors")
 'next
 (blank)
 (colorize
  (para #:align 'center "Soundness fails" sym:implies "bug in type rules")
  BlueColor))

(slide
 #:title "Type Soundness"
 (para #:width WIDE (code letrec) "checking has a bug:")
 (code {letrec {[f : (num -> num)
                   f]}
         {f 10}})
 'next
 (blank)
 (blank)
 (para #:width WIDE "Solution 1: change the grammar for" (code letrec))
 (scale
  (grammar-table
   (list (nonterm "Exp") eqls (t "...") (blank)
         (blank)           -or- (code {letrec {[#,(nonterm "Symbol") : #,(nonterm "Type") 
                                                #||#     {lambda {[#,(nonterm "Symbol") : #,(nonterm "Type")]}
                                                           #,(nonterm "Exp")}]}
                                        #,(nonterm "Exp")})
         (blank)))
  0.8)
 'next
 (para #:width WIDE "Solution 2: adjust the soundness theorem to allow a run-time error"))

(slide
 #:title "Type Soundness"
 (para #:width (get-client-w #:aspect 'fullscreen) (code let-type) "checking has a bug, too:")
 (scode {{let-type [Foo {a num} {b num}]
           {lambda {x : Foo} {+ 1 {type-case Foo x
                                    [{a n} n]
                                    [{b n} n]}}}}
         {let-type [Foo {a (num -> num)} {b num}]
           {a {lambda {[y : num]} y}}}})
 'next
 (blank)
 (para #:width (get-client-w #:aspect 'fullscreen) "Solution 1: no local type declarations")
 'next
 (para #:width (get-client-w #:aspect 'fullscreen) "Solution 2: don't let" (Q) "escape" (code let-type))
 (withtype-rule
  (lambda () (list
              (hbl-append (Q) (t " not in ") (T 0))))))

