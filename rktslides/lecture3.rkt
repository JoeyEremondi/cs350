#lang slideshow
(require "utils/colors.ss"
         "utils/utils.ss"
         "utils/alg.ss"
         slideshow/code
         slideshow/balloon
         racket/math)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 1"))

(define (show-eval expr val)
  (hbl-append (* 2 gap-size) expr sym:implies val))


(define old-datatype
  (scode
   (define-type Exp
     (numE [n : Number])
     (idE [s : Symbol])
     (plusE [l : Exp] [r : Exp])
     (multE [l : Exp] [r : Exp])
     (appE [s : Symbol] [arg : Exp]))))

(define new-datatype
  (scode
   (define-type Exp
     (numE [n : Number])
     (idE [s : Symbol])
     (plusE [l : Exp] [r : Exp])
     (multE [l : Exp] [r : Exp])
     (appE [s : Symbol] [arg : Exp])
     (letE [n : Symbol] [rhs : Exp]
           [body : Exp]))))

(define old-grammar
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)         -or- (code #,(nonterm "Symbol")) (blank)
        (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {#,(nonterm "Symbol") #,(nonterm "Exp")}) (blank)))


(slide
 #:title "Curly with Arithmetic and Functions"
 'alts
 (list
  (list
   (code
    (code:comment "An EXP is either")
    (code:comment " - `NUMBER")
    (code:comment " - `SYMBOL")
    (code:comment " - `{+ EXP EXP}")
    (code:comment " - `{* EXP EXP}")
    (code:comment " - `{SYMBOL EXP}"))
   'next
   (blank)
   'alts
   (let ([p (grammar-table old-grammar)])
     (list
      (list p)
      (list (refocus (hc-append gap-size
                                p
                                (vl-append (current-line-sep)
                                           (dt "Backus Naur Form")
                                           (para #:fill? #f "(" (dt "BNF") " )")))
                     p)))))
  (list
   (grammar-table old-grammar)
   'next
   (blank)
   old-datatype)
  #;
  (list
   (grammar-table old-grammar))))

(slide
 #:title "Curly with Local Definitions"
 (grammar-table
  (append
   old-grammar
   (list (blank)          -or- (code {let {[#,(nonterm "Symbol") #,(nonterm "Exp")]} 
                                       #,(nonterm "Exp")}) 
         new-label)))
 'next
 (blank)
 'alts
 (list
  (list (show-eval (code {let {[x {+ 1 2}]}
                           {+ x x}})
                   (code 6)))
  (list (show-eval (code {+ {let {[x {+ 1 2}]}
                              {+ x x}}
                            1})
                   (code 7)))
  (list (show-eval (code {+ {let {[x {+ 1 2}]}
                              {+ x x}}
                            {let {[x {- 4 3}]}
                              {+ x x}}})
                   (code 8)))
  
  (list (show-eval (code {+ {let {[x {+ 1 2}]}
                              {+ x x}}
                            {let {[y {- 4 3}]}
                              {+ y y}}})
                   (code 8)))
  (list (show-eval (code {let {[x {+ 1 2}]}
                           {let {[x {- 4 3}]}
                             {+ x x}}})
                   (code 2)))
  
  (list (show-eval (code {let {[x {+ 1 2}]}
                           {let {[y {- 4 3}]}
                             {+ x x}}})
                   (code 6)))
  
  (list (show-eval (code {let {[x {+ 1 2}]}
                           {let {[x {- 4 x}]}
                             {+ x x}}})
                   (code 2)))
  (list (blank))
  (list new-datatype)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 2"))

(define parsing-let-title (hbl-append (titlet "Parsing ") (code let)))

(slide
 #:title parsing-let-title

 (scode
  (code:comment "An EXP is either ...")
  (code:comment
   "- `{let {[SYMBOL EXP]})")
  (code:comment
   "      EXP}"))
 'next
 (blank)
 (code
  (s-exp-match? `{let {[SYMBOL ANY]} ANY} s))
 'next
 (blank gap-size)
 'alts
 (let ([mk
        (lambda (#:f? [f? #f]
                 #:e? [e? f?]
                 #:d? [d? e?]
                 #:c? [c? d?]
                 #:b? [b? c?]
                 #:a? [a? b?]
                 #:tag? [tag? #t]
                 #:a-tag [a-tag #f]
                 #:b-tag [b-tag #f]
                 #:c-tag [c-tag #f]
                 #:d1-tag [d1-tag #f]
                 #:d2-tag [d2-tag #f]
                 #:d3-tag [d3-tag #f]
                 #:e-tag [e-tag #f])
          (let* ([a (code (second
                           (s-exp->list s)))]
                 [g-a (ghost a)]
                 [b (code (first
                           (s-exp->list #,g-a)))]
                 [g-b (ghost b)]
                 [d1 (code (first bs))]
                 [d2 (code (second bs))]
                 [d3 (code (third (s-exp->list s)))]
                 [g-d1 (ghost d1)]
                 [g-d2 (ghost d2)]
                 [g-d3 (ghost d3)]
                 [e (code (s-exp->symbol #,g-d1))]
                 [g-e (ghost e)]
                 [f (code
                     (letE #,g-e
                           (parse #,g-d2)
                           (parse #,g-d3)))]
                 [g-f (ghost f)]
                 [tag (lambda (p t [spike 'ne] [dx gap-size] [dy 0] [find (lambda (p b)
                                                                            (define-values (x y) (lt-find p b))
                                                                            (values x (+ y 16)))])
                        (if (and tag? t)
                            (refocus (pin-balloon (wrap-balloon t spike dx dy)
                                                  p
                                                  p find)
                                     p)
                            p))]
                 [bs (tag (code bs) c-tag 'nw 0 (* -2 gap-size) cb-find)]
                 [c (code
                     (let ([#,bs (s-exp->list #,g-b)])
                       #,g-f))]
                 [p (if c? c (ghost c))]
                 [p (if a? (pin-over p g-a lt-find (tag a a-tag 'ne gap-size (- 16))) p)]
                 [p (if b? (pin-over p g-b lt-find (tag b b-tag 'ne gap-size (- 16))) p)]
                 [p (if d? (pin-over p g-d1 lt-find (tag d1 d1-tag 'se)) p)]
                 [p (if d? (pin-over p g-d2 lt-find (tag d2 d2-tag)) p)]
                 [p (if d? (pin-over p g-d3 lt-find (tag d3 d3-tag 'ne gap-size -5 lb-find)) p)]
                 [p (if e? (pin-over p g-e lt-find (tag e e-tag 'se)) p)]
                 [p (if f? (pin-over p g-f lt-find f) p)])
            (list p)))])
   (define-syntax-rule (se-code e ...)
     (colorize
      (parameterize ([code-colorize-enabled #f])
        (code e ...))
      (current-comment-color) ))
   (let ([a-tag (se-code `{[SYMBOL EXP]})]
         [b-tag (se-code `[SYMBOL EXP])]
         [c-tag (code (list #,(se-code `SYMBOL) #,(se-code `EXP)))])
     (list
      (mk #:a? #t)
      (mk #:a? #t
          #:a-tag a-tag)
      (mk #:b? #t
          #:a-tag a-tag)
      (mk #:b? #t
          #:b-tag b-tag)
      (mk #:c? #t
          #:b-tag b-tag)
      (mk #:c? #t
          #:c-tag c-tag)
      (mk #:d? #t
          #:d1-tag (se-code `SYMBOL)
          #:d2-tag (se-code `EXP)
          #:d3-tag (se-code `EXP))
      (mk #:e? #t
          #:e-tag (se-code SYMBOL)
          #:d2-tag (se-code `EXP)
          #:d3-tag (se-code `EXP))
      (mk #:f? #t)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 3"))

(slide
 #:title "Substitution"
 (para (code (interp (parse `{let {[x 8]}
                               {+ x x}})
                     ....)))
 'next
 (para (htl-append
        gap-size
        sym:implies
        (code (interp (subst (parse `8)
                             'x
                             (parse `{+ x x}))
                      ....)))))

(define (example prob #:thought [thought #f] ans test)
  (define (add-g l) (if thought (cons ghost l) l))
  (map (lambda (a t th)
         (list (para
                (vl-append
                 (current-line-sep)
                 (hbl-append (scode (code:comment "")) 
                             (if (and thought (eq? th values))
                                 (refocus (pin-balloon 
                                           (wrap-balloon thought 'n 0 (- gap-size))
                                           prob
                                           prob
                                           cb-find)
                                          prob)
                                 prob)
                             (tt " ") (a sym:implies) (tt " ") (a ans))
                 (t test)))))
       (add-g (list ghost values values))
       (add-g (list ghost ghost values))
       (add-g (list values ghost ghost))))

(define subs-local-title "Sustitutions and Local Binding")

(define first-example-alts
  (example (scode (code:line 10 for x  in  {let {[y 17]} x}))
           (scode {let {[y 17]} 10})
           (scode (test (subst (numE 10) 'x (letE 'y (numE 17) (idE 'x)))
                        (letE 'y (numE 17) (numE 10))))))

(define second-example-alts
  (example (scode (code:line 10 for x  in  {let {[y x]} y}))
           (scode {let {[y 10]} y})
           (scode (test (subst (numE 10) 'x (letE 'y (idE 'x) (idE 'y)))
                        (letE 'y (numE 10) (idE 'y))))))

(define third-example-alts
  (example (scode (code:line 10 for x  in  {let {[x y]} x}))
           #:thought (scode {let {[x {+ 5 5}]}
                              {let {[x y]}
                                x}})
           (scode {let {[x y]} x})
           (scode (test (subst (numE 10) 'x (letE 'x (idE 'y) (idE 'x)))
                        (letE 'x (idE 'y) (idE 'x))))))

(define last-example-alts
  (example (scode (code:line 10 for x  in  {let {[x x]} x}))
           #:thought (scode {let {[x {+ 5 5}]}
                              {let {[x x]}
                                x}})
           (scode {let {[x 10]} x})
           (scode (test (subst (numE 10) 'x (letE 'x (idE 'x) (idE 'x)))
                        (letE 'x (numE 10) (idE 'x))))))

(define other-example-alts
  (example (scode (code:line 10 for x  in  {let {[y x]} x}))
           (scode {let {[y 10]} 10})
           (scode (test (subst (numE 10) 'x (letE 'y (idE 'x) (idE 'x)))
                        (letE 'y (numE 10) (numE 10))))))

(slide
 #:title subs-local-title
 'alts~
 first-example-alts
 'next (blank) 'alts~
 second-example-alts
 'next (blank) 'alts~
 third-example-alts
 'next (blank) 'alts~
 last-example-alts)

(define (subst-code inside)
  (para
   (scale
    (ct-superimpose
     (code
      (define (subst [what : Exp] [for : Symbol] [in : Exp])
        (type-case Exp in
          ....
          [(letE n rhs body)
           #,inside])))
     (inset (apply vc-append gap-size
                   (append (last other-example-alts)
                           (last third-example-alts)))
            0 (* client-h 0.6) 0 0))
    0.8)))

(slide
 #:title subs-local-title
 'alts
 (list
  (list (subst-code (code ....)))
  (list (subst-code (code (letE ....))))
  (list (subst-code (code (letE n
                                ....))))
  (list (subst-code (code (letE n
                                (subst what for rhs)
                                ....))))
  (list (subst-code (code (letE n
                                (subst what for rhs)
                                (if (symbol=? n for)
                                    ....
                                    ....)))))
  (list (subst-code (code (letE n
                                (subst what for rhs)
                                (if (symbol=? n for)
                                    body
                                    ....)))))
  (list (subst-code (code
                     (letE n
                           (subst what for rhs)
                           (if (symbol=? n for)
                               body
                               (subst what for body))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 4"))

(define (align . args)
  (apply para #:width (get-client-w #:aspect 'fullscreen) args))

(slide
 #:title "Cost of Substitution"
 (align
  (code (interp #,(parsed
                   {let {[x 1]}
                     {let {[y 2]}
                       {+ 100 {+ 99 {+ 98 ... {+ y x}}}}}}))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed
                   {let {[y 2]}
                     {+ 100 {+ 99 {+ 98 ... {+ y 1}}}}}))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed {+ 100 {+ 99 {+ 98 ... {+ 2 1}}}}))))
 'next
 (blank)
 (blank)
 (colorize
  (para #:align 'center
        "With" (bt "n") "variables, evaluation will take" 
        (hbl-append (t "O(")
                    (bt "n") 
                    (text "2" `(superscript . ,(current-main-font)) (current-font-size))
                    (t ")"))
        "time!")
  BlueColor))

(define sym:emptyset (blank))

(define % (tt ""))

(slide
 #:title "Deferring Substitution"
 (align
  (code (interp #,(parsed+sub
                   {let {[x 1]}
                     {let {[y 2]}
                       {+ 100 {+ 99 {+ 98 ... {+ y x}}}}}}
                   code:blank))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed+sub
                   {let {[y 2]}
                     {+ 100 {+ 99 {+ 98 ... {+ y x}}}}}
                   (code:line x = 1)))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed+sub
                   {+ 100 {+ 99 {+ 98 ... {+ y x}}}}
                   (code:line y = 2 #,% x = 1)))))
 'next (align sym:implies (code ...) sym:implies)
 (align
  (code (interp #,(parsed+sub y
                              (code:line y = 2 #,% x = 1))))))


(slide
 #:title "Deferring Substitution with the Same Identifier"
 (align
  (code (interp #,(parsed+sub
                   {let {[x 1]}
                     {let {[x 2]}
                       x}}
                   code:blank))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed+sub
                   {let {[x 2]}
                     x}
                   (code:line x = 1)))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed+sub
                   x
                   (code:line x = 2 #,% x = 1)))))
 'next
 (blank)
 (para #:align 'center "Always add to start, then always check from start"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 5"))

(slide
 #:title "Representing Deferred Substitution: Environments"
 (para #:align 'center "Change")
 (item #:bullet (ghost bullet)
       (code interp : (Exp (Listof Func-Defn) -> Number)))
 (para #:align 'center"to")
 (item #:bullet (ghost bullet)
       (code interp : (Exp Env (Listof Func-Defn) -> Number)))
 'next
 (blank)
 (code
  mt-env : Env
  extend-env : (Binding Env -> Env)
  bind : (Symbol Number -> Binding)
  lookup : (Symbol Env -> Number))
 'next
 (blank)
 (blank)
 'alts
 (list
  (list 
   (ht-append gap-size
              (balloon-pict (balloon-code code:blank))
              (scode mt-env)))
  (list
   (ht-append gap-size
              (balloon-pict (balloon-code x = 1))
              (scode (extend-env (bind 'x 1)
                                 mt-env))))
  (list
   (ht-append gap-size
              (balloon-pict (balloon-code y = 2 #,% x = 1))
              (scode (extend-env (bind 'y 2)
                                 (extend-env (bind 'x 1)
                                             mt-env)))))))

(slide
 #:title "Environments"
 (code
  (define-type Binding
    (bind [name : Symbol]
          [val : Number]))
  code:blank
  (define-type-alias Env (Listof Binding))
  code:blank
  (define mt-env empty)
  (define extend-env cons)))

(define final-lookup
  (code
   (define (lookup [n : Symbol] [env : Env]) : Number
     (type-case (Listof Binding) env
       [empty (error 'lookup "free variable")]
       [(cons b rst-env) (cond
                           [(symbol=? n (bind-name b))
                            (bind-val b)]
                           [else (lookup n rst-env)])]))))

(define (make-lookup #:cons-cond? [cons-cond? #f]
                     #:cons-compare? [cons-compare? cons-cond?]
                     #:cons-template? [cons-template? cons-compare?]
                     #:empty-case? [empty-case? cons-template?]
                     #:body? [body? empty-case?])
  (code
   #,(lt-superimpose
      (ghost final-lookup)
      (code
       (define (lookup [n : Symbol] [env : Env]) : Number
         #,(if (not body?)
               (code ....)
               (code
                (type-case (Listof Binding) env
                  [empty #,(if empty-case?
                               (code (error 'lookup "free variable"))
                               (code ....))]
                  [(cons b rst-env) #,(if (not cons-template?)
                                          (code ....)
                                          (let* ([rec (code (lookup n rst-env))]
                                                 [g-rec (ghost rec)]
                                                 [b (code b)]
                                                 [g-b (ghost b)]
                                                 [comp (code (symbol=? n (bind-name #,g-b)))]
                                                 [g-comp (ghost comp)]
                                                 [cnd (code
                                                       (cond
                                                         [#,g-comp
                                                          (bind-val b)]
                                                         [else #,g-rec]))]
                                                 [g-cnd (ghost cnd)])
                                            (let* ([p g-cnd]
                                                   [p (if (not cons-cond?)
                                                          (refocus (ltl-superimpose p (code ....))
                                                                   p)
                                                          p)]
                                                   [p (if cons-template?
                                                          (pin-over p
                                                                    g-rec lt-find
                                                                    rec)
                                                          p)]
                                                   [p (if cons-template?
                                                          (pin-over p
                                                                    g-b lt-find
                                                                    b)
                                                          p)]
                                                   [p (if cons-compare?
                                                          (pin-over p
                                                                    g-comp lt-find
                                                                    comp)
                                                          p)]
                                                   [p (if cons-cond?
                                                          (pin-over p
                                                                    g-cnd lt-find
                                                                    cnd)
                                                          p)])
                                              p)))]))))))
   code:blank
   code:blank
   (test/exn (lookup 'x mt-env)
             "free variable")
   (test (lookup 'x (extend-env (bind 'x 1) empty-env))
         1)
   (test (lookup 'x (extend-env (bind 'y 1)
                                (extend-env (bind 'x 2) empty-env)))
         2)))

(slide
 #:title "Environment Lookup"
 'alts
 (list
  (list (make-lookup))
  (list (make-lookup #:body? #t))
  (list (make-lookup #:empty-case? #t))
  (list (make-lookup #:cons-template? #t))
  (list (make-lookup #:cons-compare? #t))
  (list (make-lookup #:cons-cond? #t))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 6"))

(define (align+implies x)
  (item #:bullet sym:implies
        #:width (get-client-w #:aspect 'fullscreen)
        x))

(define interp-with-env-title "Interp with Environments")

(slide
 #:title interp-with-env-title
 (align
  (scode (interp #,(parsed
                    {let {[x 1]}
                      {let {[y 2]}
                        {+ 100 {+ 99 {+ 98 ... {+ y x}}}}}})
                 mt-env)))
 'next
 (align+implies
  (scode (interp #,(parsed
                    {let {[y 2]}
                      {+ 100 {+ 99 {+ 98 ... {+ y x}}}}})
                 (extend-env (bind 'x 1) mt-env))))
 'next 
 (align+implies
  (scode (interp #,(parsed
                    {+ 100 {+ 99 {+ 98 ... {+ y x}}}})
                 (extend-env (bind 'y 2)
                             (extend-env (bind 'x 1) 
                                         mt-env)))))
 'next
 (inset
  (vc-append
   (current-line-sep)
   (align sym:implies (code ...))
   (align+implies
    (scode (interp #,(parsed y) (extend-env (bind 'y 2)
                                            (extend-env (bind 'x 1) 
                                                        mt-env))))))
  0 (- gap-size) 0 0))

(define app-pattern (code (appE s arg)))
(define let-pattern (code (letE n rhs body)))
(define fd-bind (code fd))

(define example-color "lightblue")

(define (wae-interp with-case id-case [fun-case (code ...)]
                    #:example-at [at-pattern #f]
                    #:example [example #f]
                    #:example2 [example2 #f])
  (align
   (scale
    (let ([p (code
              (define (interp [a : Exp] [env : Env] [defs : (Listof Func-Defn)])
                (type-case Exp a
                  [(numE n) n]
                  [(idE s) #,id-case]
                  [(plusE l r) (+ (interp l env defs) (interp r env defs))]
                  [(multE l r) (* (interp l env defs) (interp r env defs))]
                  [#,app-pattern (local [(define #,fd-bind (get-fundef s defs))]
                                   #,fun-case)]
                  [#,let-pattern #,with-case])))])
      (if example
          (let ([p (pin-balloon (wrap-balloon example 'ne 0 (- gap-size) example-color)
                                p
                                at-pattern cb-find)])
            (if example2
                (pin-balloon (wrap-balloon example2 's 0 gap-size example-color)
                             p
                             fd-bind ct-find)
                p))
          p))
    0.8)))

(define lookup-call (code (lookup s env)))

(define (place view? gen inner)
  (define g (ghost (launder inner)))
  (define-values (norm dots) (gen g))
  (define p (pin-over ((if view? values ghost) norm) g lt-find inner))
  (if view?
      p
      (lt-superimpose p dots)))

(define (with-case #:interp? [interp? #f]
                   #:extend? [extend? interp?]
                   #:bind? [bind? extend?])
  (place
   interp?
   (lambda (x)
     (values
      (code (interp body
                    #,x
                    defs))
      (code ...
            code:blank
            code:blank
            code:blank
            ...)))
   (place
    extend?
    (lambda (x)
      (values
       (code
        (extend-env
         #,x
         env))
       (code
        ...
        code:blank
        ...)))
    (place
     bind?
     (lambda (x)
       (values
        (code (bind n #,x))
        (code ...)))
     (code (interp rhs env defs))))))

(define with-rhs (with-case #:interp? #t))

(define (app-case #:extend? [extend? #f]
                  #:interp? [interp? extend?]
                  #:bind? [bind? interp?])
  (place
   interp?
   (lambda (x)
     (values
      (code (interp (fd-body fd)
                    #,x
                    defs))
      (code ...
            code:blank
            ...)))
   (place
    extend?
    (lambda (x)
      (values
       (code (extend-env
              #,x
              mt-env))
       (code ...
             code:blank
             ...)))
    (place
     bind?
     (lambda (x)
       (values
        (code (bind (fd-arg fd)
                    #,x))
        (code ...)))
     (code (interp arg env defs))))))

(define let-interp-example (code {let {[x {+ 1 2}]}
                                   {* x x}}))
(define app-interp-example (code {f {+ 1 2}}))
(define defn-interp-example (code {define {f x}
                                    {* x x}}))

(slide
 #:title interp-with-env-title
 'alts
 (list (list (wae-interp (code ...) (code ...)))
       (list (wae-interp (code ...)
                         lookup-call))
       (list (wae-interp (code ...)
                         lookup-call
                         #:example-at let-pattern
                         #:example let-interp-example))
       (list (wae-interp (with-case)
                         lookup-call
                         #:example-at let-pattern
                         #:example let-interp-example))
       (list (wae-interp (with-case #:bind? #t)
                         lookup-call
                         #:example-at let-pattern
                         #:example let-interp-example))
       (list (wae-interp (with-case #:extend? #t)
                         #:example-at let-pattern
                         #:example let-interp-example
                         lookup-call))
       (list (wae-interp with-rhs
                         lookup-call
                         #:example-at let-pattern
                         #:example let-interp-example))
       (list (wae-interp with-rhs
                         lookup-call
                         #:example-at app-pattern
                         #:example app-interp-example
                         #:example2 defn-interp-example))
       (list (wae-interp with-rhs
                         lookup-call
                         (app-case)
                         #:example-at app-pattern
                         #:example app-interp-example
                         #:example2 defn-interp-example))
       (list (wae-interp with-rhs
                         lookup-call
                         (app-case #:bind? #t)
                         #:example-at app-pattern
                         #:example app-interp-example
                         #:example2 defn-interp-example))
       (list (wae-interp with-rhs
                         lookup-call
                         (app-case #:interp? #t)
                         #:example-at app-pattern
                         #:example app-interp-example
                         #:example2 defn-interp-example))))

(slide
 #:title "Function Calls"
 (align
  (code {define {bad x} {+ x y}}))
 (blank)
 (align
  (code (interp #,(parsed+sub
                   {let {[y 2]}
                     {bad 10}}
                   code:blank))))
 'next (align sym:implies)
 (align
  (code (interp #,(parsed+sub
                   {bad 10}
                   (code:line y = 2)))))
 'next (align sym:implies)
 'alts
 (list (list
        (align
         (code (interp #,(parsed+sub
                          {+ x y}
                          (code:line ...))))))
       (list
        (align
         (code (interp #,(parsed+sub
                          {+ x y}
                          (code:line x = 10)))))))
 (blank)
 (blank)
 (para #:align 'center
       "Interpreting function body starts with only one substitution"))

(slide
 #:title interp-with-env-title
 'alts
 (list (list (wae-interp with-rhs
                         lookup-call
                         (app-case #:interp? #t)))
       (list (wae-interp with-rhs
                         lookup-call
                         (app-case #:extend? #t)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide (titlet "Part 7"))

(define binding-color "lightblue")
(define bound-color "yellowgreen")
(define free-color "pink")

(define (binding-slide #:free-alt? [free-alt? #f]
                       #:free-hilite? [free-hilite? free-alt?]
                       #:free-example? [free-example? free-hilite?]
                       #:bound-alt? [bound-alt? free-example?]
                       #:bound-hilite? [bound-hilite? bound-alt?]
                       #:bound-example? [bound-example? bound-hilite?]
                       #:binding-alt? [binding-alt? bound-example?]
                       #:binding-hilite? [binding-hilite? binding-alt?]
                       #:binding-example? [binding-example? binding-hilite?])
  (define (show v?) (if v? values ghost))
  (slide
   #:title "Binding Terminology"
   (para (bit "binding") "---" "where an identifier gets its meaning")
   ((show binding-example?)
    (code {let {[#,(hilite #:on? binding-hilite? (code x) #:color binding-color) 5]} ....}))
   ((show binding-alt?)
    (code {define {f #,(hilite #:on? binding-hilite? (code x) #:color binding-color)} ....}))
   (blank)
   (para (bit "bound") "---" "refers to a binding")
   ((show bound-example?)
    (code {let {[x 5]} .... #,(hilite #:on? bound-hilite? (code x) #:color bound-color) ....}))
   ((show bound-alt?)
    (code {define {f x} .... #,(hilite #:on? bound-hilite? (code x) #:color bound-color) ....}))
   (blank)
   (para (bit "free") "---" "does not have a binding")
   ((show free-example?)
    (code {let {[x 5]} .... #,(hilite #:on? free-hilite? (code y) #:color free-color) ....}))
   ((show free-alt?)
    (code {define {f x} .... #,(hilite #:on? free-hilite? (code y) #:color free-color) ....}))))

(when condense?
  (skip-slides 9))
(unless condense?
  (binding-slide)
  (binding-slide #:binding-example? #t)
  (binding-slide #:binding-hilite? #t)
  (binding-slide #:binding-alt? #t)
  (binding-slide #:bound-example? #t)
  (binding-slide #:bound-hilite? #t)
  (binding-slide #:bound-alt? #t)
  (binding-slide #:free-example? #t)
  (binding-slide #:free-hilite? #t))
(binding-slide #:free-alt? #t)

(define (add-bind-arrow p x1 x2)
  (pin-arrow-line (/ gap-size 2)
                  p
                  x2 ct-find
                  x1 ct-find
                  #:start-angle (* pi 1/4)
                  #:start-pull 1
                  #:end-angle (* pi -1/2)
                  #:line-width 4
                  #:color bound-color))

(slide
 #:title "Free and Bound"
 'alts
 (list
  (list
   (code {let {[x 5]}
           {let {[y x]}
             {+ y {+ z x}}}}))
  (list
   (code {let {[#,(hilite (code x) #:color binding-color) 5]}
           {let {[#,(hilite (code y) #:color binding-color) #,(hilite (code x) #:color bound-color)]}
             {+ #,(hilite (code y) #:color bound-color) {+ #,(hilite (code z) #:color free-color) #,(hilite (code x) #:color bound-color)}}}}))
  (list
   (let ([mk (lambda (p)
               (code {let {[x 5]}
                       #,p}))])
     (define inner
       (code {let {[#,(hilite (code y) #:color binding-color) #,(hilite (code x) #:color free-color)]}
               {+ #,(hilite (code y) #:color bound-color) {+ #,(hilite (code z) #:color free-color) #,(hilite (code x) #:color free-color)}}}))
     (define b (cellophane (mk (ghost inner)) 0.3))
     (pin-over b inner lt-find inner)))
  (list
   (code {let {[#,(hilite (code x) #:color binding-color) 5]}
           {let {[#,(hilite (code x) #:color binding-color) #,(hilite (code x) #:color bound-color)]}
             {+ #,(hilite (code y) #:color free-color) {+ #,(hilite (code z) #:color free-color) #,(hilite (code x) #:color bound-color)}}}}))
  (list
   (let ([mk (lambda (p)
               (code {let {[x 5]}
                       #,p}))])
     (define inner
       (code {let {[#,(hilite (code x) #:color binding-color) #,(hilite (code x) #:color free-color)]}
               {+ #,(hilite (code y) #:color free-color) {+ #,(hilite (code z) #:color free-color) #,(hilite (code x) #:color bound-color)}}}))
     (define b (cellophane (mk (ghost inner)) 0.3))
     (pin-over b inner lt-find inner)))
  (list
   (let ([x1 (code x)]
         [x2 (code x)]
         [x3 (code x)]
         [x4 (code x)])
     (define p
       (code {let {[#,(hilite x1 #:color binding-color) 5]}
               {let {[#,(hilite x3 #:color binding-color) #,(hilite x2 #:color bound-color)]}
                 {+ #,(hilite (code y) #:color free-color) {+ #,(hilite (code z) #:color free-color) #,(hilite x4 #:color bound-color)}}}}))
     (define p2 (add-bind-arrow p x1 x2))
     (pin-arrow-line (/ gap-size 2)
                     p2
                     x4 cb-find
                     x3 lb-find
                     #:start-angle (* pi -1/2)
                     #:end-angle (* pi 1/4)
                     #:line-width 4
                     #:color bound-color)))
  (list
   (code {define {double x} {+ x x}}
         code:blank
         {double 3}))
  (list
   (let ([double1 (code double)]
         [double2 (code double)]
         [x1 (code x)]
         [x2 (code x)]
         [x3 (code x)])
     (define rhs (code {+ #,(hilite x2 #:color bound-color) #,(hilite x3 #:color bound-color)}))
     (let* ([p (code {define {#,(hilite double1 #:color binding-color) #,(hilite x1 #:color binding-color)} #,rhs}
                    code:blank
                    {#,(hilite double2 #:color bound-color) 3})]
            [p (add-bind-arrow p x1 x2)]
            [p (add-bind-arrow p x1 x3)]
            [p (add-bind-arrow p double1 double2)])
       p)))))
