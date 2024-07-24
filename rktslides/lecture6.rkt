#lang slideshow
(require slideshow/code
         slideshow/play
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss"
         "utils/crate.rkt")

(define (align . l) (apply para #:width (* 0.95 (get-client-w #:aspect 'fullscreen)) l))
(define sep-line (colorize (hline (* (get-client-w #:aspect 'fullscreen) 1/2) 0) "blue"))

(slide (titlet "Part 1"))

(define box-v-var-title "Boxes vs. Variables")

(slide
 #:title box-v-var-title
 (item "State via box:")
 (code {let {[x {box 1}]}
         {begin
           {set-box! x 2}
           {unbox x}}})
 'next
 (blank)
 (item "State via variable:")
 (code {let {[x 1]}
         {begin
           {set! x 2}
           x}}))

(define (box-vs-var-slide #:bind? [bind? #f]
                          #:y-box? [y-box? bind?]
                          #:f-box? [f-box? y-box?]
                          #:hilite? [hilite? f-box?]
                          #:boxes? [boxes? hilite?])
  (define (x-hi p) (hilite #:on? hilite? p))
  (define (f-hi p) (hilite #:on? f-box? #:color "beige" p))
  (define (y-hi p) (hilite #:on? y-box? #:color "pink" p))
  (define y-ref (if y-box? (y-hi (code {unbox y})) (code y)))
  (define one-arg (if y-box? (y-hi (code {box 1})) (code 1)))
  (define (add-code p)
    (if bind?
        (rb-superimpose
         p
         (inset (frame
                 (inset
                  (scode
                   (define-type Binding
                     (bind [name : Symbol]
                           [location : Location])))
                  (/ gap-size 4)))
                0 0 (- gap-size) (- gap-size)))
        p))
  (slide
   #:title box-v-var-title
   (add-code
    (align
     (code
      {let {[x #,(x-hi (code 5))]}
        {let {[f #,(f-hi (code {lambda {y}
                                 {+ #,(x-hi (code x)) #,(y-hi (code y))}}))]}
          {begin
            #,(hilite #:on? hilite? (code {set! x 6}))
            {#,(f-hi (code f)) #,(y-hi (code 1))}}}})))
   (blank)
   ((if boxes? values ghost)
    sep-line)
   (blank)
   ((if boxes? values ghost)
    (align
     (code
      {let {[x #,(x-hi (code {box 5}))]}
        {let {[f #,(let ([f (code
                             {lambda {y}
                               {+ #,(x-hi (code {unbox x})) #,y-ref}})])
                     (if f-box?
                         (f-hi (code {box #,f}))
                         f))]}
          {begin
            #,(x-hi (code {set-box! x 6}))
            {#,(if f-box? (f-hi (code {unbox f})) (code f)) #,one-arg}}}})))))

(box-vs-var-slide)
(box-vs-var-slide #:boxes? #t)
(box-vs-var-slide #:hilite? #t)
(box-vs-var-slide #:f-box? #t)
(box-vs-var-slide #:y-box? #t)
(box-vs-var-slide #:bind? #t)

;; ----------------------------------------

(slide (titlet "Part 2"))

(slide
 #:title "Variables"
 (grammar-table
  (list (nonterm "Exp") eqls (nonterm "Number") (blank)
        (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {- #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code #,(nonterm "Symbol")) (blank)
        (blank)         -or- (code {lambda {#,(nonterm "Symbol")} #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
        (blank)         -or- (code {set! #,(nonterm "Exp") #,(nonterm "Exp")}) new-label
        (blank)         -or- (code {begin #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)))
 'next
 (blank)
 (blank)
 (para
  #:align 'center
  (hbl-append gap-size
              (code {let {[b 0]}
                      {begin
                        {set! b 10}
                        b}})
              sym:implies
              (code 10))))

;; ----------------------------------------

(define examples-title "Variable Examples")

(define (sto-exp p)
  (list
   (align
    (code interp : (Exp Env Store -> Result)
          code:blank
          #,p))))

(slide
 #:title examples-title
 'alts
 (list
  (sto-exp (code (test (interp (numE 5) mt-env mt-store)
                       (v*s (numV 5) mt-store))))
  (sto-exp (code (test (interp (parse `{let {[x 5]} x})
                               mt-env
                               mt-store)
                       (v*s ...
                            ...))))
  (sto-exp (code (test (interp (parse `{let {[x 5]} x})
                               mt-env
                               mt-store)
                       (v*s (numV 5)
                            ...))))
  (sto-exp (code (test (interp (parse `{let {[x 5]} x})
                               mt-env
                               mt-store)
                       (v*s (numV 5)
                            ... (cell 1 (numV 5)) ...))))
  (sto-exp (code (test (interp (parse `{let {[x 5]} x}) 
                               mt-env 
                               mt-store)
                       (v*s (numV 5)
                            (override-store
                             (cell 1 (numV 5))
                             mt-store)))))
  (sto-exp (code (test (interp (parse `x)
                               ...
                               ...)
                       ...)))
  (sto-exp (code (test (interp (parse `x)
                               (extend-env (bind 'x ...)
                                           mt-env)
                               ...)
                       ...)))
  (sto-exp (code (test (interp (parse `x)
                               (extend-env (bind 'x 1)
                                           mt-env)
                               (override-store (cell 1 ...)
                                               mt-store))
                       ...)))
  (sto-exp (code (test (interp (parse `x)
                               (extend-env (bind 'x 1)
                                           mt-env)
                               (override-store (cell 1 (numV 5))
                                               mt-store))
                       (v*s (numV 5)
                            ...))))
  (sto-exp (code (test (interp (parse `x)
                               (extend-env (bind 'x 1)
                                           mt-env)
                               (override-store (cell 1 (numV 5))
                                               mt-store))
                       (v*s (numV 5)
                            (override-store (cell 1 (numV 5))
                                            mt-store)))))
  (sto-exp (code (test (interp (parse `{{lambda {x} {+ x x}}
                                        8})
                               mt-env
                               mt-store)
                       (v*s ...
                            ...))))
  (sto-exp (code (test (interp (parse `{{lambda {x} {+ x x}}
                                        8})
                               mt-env
                               mt-store)
                       (v*s (numV 16)
                            ...))))
  (sto-exp (code (test (interp (parse `{{lambda {x} {+ x x}}
                                        8})
                               mt-env
                               mt-store)
                       (v*s (numV 16)
                            ... (cell 1 (numV 8)) ...))))
  (append
   (sto-exp (code (test (interp (parse `{{lambda {x} {+ x x}}
                                         8})
                                mt-env
                                mt-store)
                        (v*s (numV 16)
                             (override-store (cell 1 (numV 8))
                                             mt-store)))))
   (let ([hi (lambda (p)
               (hilite #:color "pink" p))])
     (list 'next
           (blank)
           sep-line
           (blank)
           (code {{lambda {x} {+ #,(hi (code {unbox x})) #,(hi (code {unbox x}))}}
                  #,(hi (code {box 8}))}))))))

;; ----------------------------------------

(slide (titlet "Part 3"))

(define impl-title (hbl-append (code interp) (titlet " for Variables")))

(define (interp p #:suffix [suffix #f] #:scale [s 0.85])
  (list
   (align
    (scale
     (let ([c (code (define interp : (Exp Env Store -> Result)
                      (lambda (a env sto)
                        ...
                        #,p
                        ...)))])
       (if suffix
           (code #,c
                 #,suffix)
           c))
     s))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code [(idE s) (v*s (lookup s env)
                       sto)]))
  (interp
   (code [(idE s) (v*s (fetch (lookup s env) sto)
                       sto)]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env (bind n v-rhs)
                           env)
               sto-rhs))]))
  (interp
   (code
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env (bind n #,(hilite (code l)))
                           env)
               sto-rhs))]))
  (interp
   (code
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env (bind n #,(hilite (code l)))
                           env)
               (override-store #,(hilite (code (cell l v-rhs)))
                               sto-rhs)))]))
  (interp
   (code 
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (let (#,(hilite (code [l (new-loc sto-rhs)])))
         (interp body
                 (extend-env (bind n l)
                             env)
                 (override-store (cell l v-rhs)
                                 sto-rhs))))]))))

(slide
 #:title impl-title
 #:layout 'top
 'alts~
 (list
  (interp
   (code
    [(setE var val)
     ...]))
  (interp
   (code
    [(setE var val)
     ... var ...
     ... (interp val env sto) ...]))
  (interp
   (code
    [(setE var val)
     (let ([l (lookup var env)])
       ... (interp val env sto) ...)]))
  (interp
   (code
    [(setE var val)
     (let ([l (lookup var env)])
       (with [(v-v sto-v) (interp val env sto)]
         ...))]))
  (interp
   (code
    [(setE var val)
     (let ([l (lookup var env)])
       (with [(v-v sto-v) (interp val env sto)]
         ... (override-store (cell l v-v)
                             sto-v)
         ...))]))
  (interp
   (code
    [(setE var val)
     (let ([l (lookup var env)])
       (with [(v-v sto-v) (interp val env sto)]
         (v*s ...
              (override-store (cell l v-v)
                              sto-v))))]))
  (interp
   (code
    [(setE var val)
     (let ([l (lookup var env)])
       (with [(v-v sto-v) (interp val env sto)]
         (v*s v-v
              (override-store (cell l v-v)
                              sto-v))))]))
  (interp
   (code
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
       (with [(v-a sto-a) (interp arg env sto-f)]
         (type-case Value v-f
           [(closV n body c-env)
            (interp body
                    (extend-env
                     (bind n v-a)
                     c-env)
                    sto-a)]
           [else (error 'interp "not a function")])))]))
  (interp
   (code
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
       (with [(v-a sto-a) (interp arg env sto-f)]
         (type-case Value v-f
           [(closV n body c-env)
            (let ([l (new-loc sto-a)])
              (interp body
                      (extend-env (bind n l)
                                  c-env)
                      (override-store (cell l v-a)
                                      sto-a)))]
           [else (error 'interp "not a function")])))]))))


;; ----------------------------------------

(slide (titlet "Part 4"))

(box-vs-var-slide #:boxes? #t)

(define fill-code
  (code
   {let {[fill! {lambda {b}
                  {set-box! b 5}}]}
     {let {[a {box 0}]}
       {begin
         {fill! a}
         {unbox a}}}}))

(define bad-fill-code
  (code
   {let {[fill?! {lambda {b}
                   {set! b 5}}]}
     {let {[a 0]}
       {begin
         {fill?! a}
         a}}}))

(define good-fill-code
  (code
   {let {[fill {lambda {b}
                 {b 5}}]}
     {let {[a 0]}
       {begin
         {fill {lambda {v} {set! a v}}}
         a}}}))

(slide
 #:title "Boxes as Values"
 'alts~
 (list
  (list
   (align fill-code))
  (list
   (align (code #,fill-code #,sym:implies 5))))
 'next
 (blank)
 sep-line
 (blank)
 'alts
 (list
  (list
   'alts~
   (list
    (list
     (align bad-fill-code))
    (list
     (align (code #,bad-fill-code
                  #,sym:implies 0)))))
  (list
   'alts~
   (list
    (list
     (align good-fill-code))
    (list
     (align (code #,good-fill-code
                  #,sym:implies 5)))))))

(slide
 #:title "Boxes as Variables and Functions"
 'alts
 (list
  (list
   (code
    (define (crate v)
      (values (lambda () v)
              (lambda (x) (set! v x))))
    code:blank
    (define (uncrate b)
      (let ([get (fst b)])
        (get)))
    code:blank
    (define (set-crate! b new-v)
      (let ([set (snd b)])
        (set new-v)))))
  (list
   (scale
    (crate-code (code
                 {let {[b {crate 0}]}
                   {begin
                     {{set-crate! b} 5}
                     {uncrate b}}}))
    0.9))))

(slide
 #:title "Boxes vs. Variables"
 (para "Mutable variables and mutable structures have the same expressive power"))

    
;; ------------------------------------------------------------

(slide (titlet "Part 5"))

(slide
 #:title "Mutating Variables"
 'alts
 (list
  (list
   (code
    (define (swap x y)
      (let ([z y])
        (set! y x)
        (set! x z)))
    code:blank
    (let ([a 10])
      (let ([b 20])
        (begin
          (swap a b)
          a))))
   'next
   (blank)
   (para #:align 'center
         "Result is" (code 10) ": assignment in"
         (code swap) "cannot affect" (code a)))
  (list
   (code 
    {let {[fill?! {lambda {b}
                    {set! b 5}}]}
      {let {[a 0]}
        {begin
          {fill?! a}
          a}}})
   'next
   (para "Result is " (code 0) "...")
   'next
   (para #:align 'right
         "but what if we want a language where the result is " (code 5) "?"))))

(define (cbr-xform cbr?)
  (vl-append
   gap-size
   (scode
    {let {[fill?! {lambda {b}
                    {set! b 5}}]}
      {let {[a 0]}
        {begin
          {fill?! a}
          a}}})
   (htl-append
    gap-size
    sym:implies
    (scode {let {[fill?! {lambda {b-b}
                           {set-box! b-b 5}}]}
             {let {[a {box 0}]}
               {begin
                 #,(if cbr?
                       (code (code:comment "{fill?! {box {unbox a}}}")
                             {fill?! a})
                        (code {fill?! {box {unbox a}}}))
                 {unbox a}}}}))))

(slide
 #:title "Call by Value"
 #:layout 'top
 (cbr-xform #f))

(slide
 #:title "Call by Reference"
 #:layout 'top
 (cbr-xform #t)
 'next
 (blank)
 (para #:align 'center
       "This is called" (dt "call by reference") ", as opposed to"
       (dt "call by value")))

(define (cbr-code #:body-n [body-n 0]
                  #:case-n [case-n 1]
                  #:id-n [id-n 1]
                  #:hilite? [hilite? #f])
  (define old
    (code (with [(v-a sto-a) (interp arg env sto-f)]
            ....)))
  (define old-1 (launder (ghost old)))
  (define old-2 (launder (ghost old)))
  (define new-body
    (fade-pict body-n
               (code ...)
               (code
                (type-case Value v-f
                  [(closV n body c-env)
                         (interp body
                                 (extend-env
                                  #,(hilite #:on? hilite? (code (bind n (lookup s env))))
                                  c-env)
                                 sto-f)]
                  [else (error ...)]))))
  (define new-case
    (cellophane
     (code
      (type-case Exp arg
        [(idE s)
         #,new-body]
        [else
         #,old-2]))
     case-n))
  (let* ([p (car
             (interp
              #:scale 1.0
              (code
               [appE (fun arg)
                     (with [(v-f sto-f) (interp fun env sto)]
                       #,(fade-pict id-n
                                    old-1
                                    new-case))])))]
         [p (slide-pict p old old-1 old-2 id-n)])
    (scale p 0.85)))

(define cbr-title "Implementing Call-by-Reference")

(void
 (play #:title cbr-title
       (lambda (n) (cbr-code #:id-n n #:case-n 0))))
(void
 (play #:title cbr-title
       #:skip-first? #t
       (lambda (n) (cbr-code #:case-n n))))
(play-n #:title cbr-title
        (lambda (n) (cbr-code #:body-n n)))
(slide
 #:title cbr-title
 (cbr-code #:body-n 1 #:hilite? #t))
