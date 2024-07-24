#lang slideshow

(require slideshow/code
         "utils/colors.ss"
         "utils/utils.ss"
         "utils/alg.ss"
         "utils/types.ss"
         "utils/tvfae-types.ss"
         (lib "list.ss"))

(current-keyword-list
 (list* "let-type"
        "LAMBDA"
        "@"
        "forall"
        (current-keyword-list)))

(define WWIDE (get-client-w  #:aspect 'fullscreen))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define blue "blue")
(define red "red")

;; ----------------------------------------
(part 1)

(define quiz-title "Inferring a Function Type")

(slide
 #:title quiz-title
 (para "What type is inferred for" (code ?) "in the following expression?")
 (blank)
 (code {let {[ident : (? -> ?)
                    {lambda {[x : ?]} x}]}
         {ident 10}})
 'next
 (blank)
 (para (colorize (bt "Answer: ") blue) (code _num)))

(slide
 #:title quiz-title
 (para "What type is inferred for" (code ?) "in the following expression?")
 (blank)
 (code {let {[ident : (? -> ?)
                    {lambda {[x : ?]} x}]}
         {ident {lambda {[y : num]} y}}})
 'next
 (blank)
 (para (colorize (bt "Answer: ") blue) (-> (code _num) (code _num))))

(define dots (colorize (t "...") RedColor))

(slide
 #:title quiz-title
 (para "What type is inferred for" (code ?) "in the following expression?")
 (blank)
 (code {let {[ident : (? -> ?)
                    {lambda {[x : ?]} x}]}
         {if #,dots
             {ident 10}
             {{ident {lambda {[y : num]} y}}
              8}}})
 'next
 (blank)
 (para (colorize (bt "Answer: ") blue) 
       "None; no single type works"
       sym:emdash "but it's a perfectly good program for any"
       dots "of type" (code bool)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Polymorphism"
 (para #:width WWIDE
       "We'd like a way to write a type that the"
       "caller chooses:")
 (blank)
 (code {let {[ident : ?
                    [LAMBDA ['a]
                      {lambda {[x : 'a]} x}]]}
         {if #,dots
             {[@ ident num] 10}
             {{[@ ident (num -> num)] {lambda {[y : num]} y}}
              8}}})
 'next
 (blank)
 (para "This" (code ident) "is" (dt "polymorphic"))
 (item "The" (code LAMBDA) "form parameterizes over a type")
 (item "The" (code @) "form picks a type"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Polymorphic Types"
 (para "What is the type of this expression?")
 'alts
 (list
  (list
   (code [LAMBDA ['a]
           {lambda {[x : 'a]} x}])
   'next
   (blank)
   (para "It should be something like" (-> (code 'a) (code 'a))
         ", but it needs a specific type before it can be used as a function"))
  (list
   (code [LAMBDA ['a]
           [LAMBDA ['b]
             {lambda {[x : 'a]} x}]])
   'next
   (blank)
   (para "It should be something like" (-> (code 'a) (code 'a))
         ", but picking" (code 'a) "gives something that still needs"
         "another type")
   'next
   (blank)
   'alts
   (let ([mk-ntf
          (lambda (A tau)
            (list
             (para "New type form:" (tt "  ") (forall A tau))
             (blank)
             (forall (code 'a) (-> (code 'a) (code 'a)))
             (forall (code 'a) (forall (code 'b) (-> (code 'a) (code 'a))))))])
     (list
      (mk-ntf (hbl-append (tt "'") (nonterm "Symbol")) (nonterm "Type"))
      (mk-ntf (A) (T)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Grammar with Polymorphism"
 (scale/improve-new-text
  (grammar-table
   (list (nonterm "Exp") eqls (nonterm "Number") (blank)
         (blank)           -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code #,(nonterm "Symbol")) (blank)
         (blank)           -or- (code {lambda {[#,(nonterm "Symbol") : #,(nonterm "Type")]} #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code {#,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)           -or- (code [LAMBDA ['#,(nonterm "Symbol")] #,(nonterm "Exp")]) new-label
         (blank)           -or- (code [@ #,(nonterm "Exp") #,(nonterm "Type")]) new-label
         (ghost (nonterm "Type")) (blank) (blank) (blank)
         (nonterm "Type") eqls (code num) (blank)
         (blank)           -or- (code (#,(nonterm "Type") -> #,(nonterm "Type"))) (blank)
         (blank)           -or- (code (forall ('#,(nonterm "Symbol")) #,(nonterm "Type"))) new-label
         (blank)           -or- (code '#,(nonterm "Symbol")) new-label))
  0.8))

;; ----------------------------------------
(part 2)

(slide
 #:title "Datatypes"
 (scode
  (define-type Exp
    ....
    (tylamE [n : Symbol]
            [body : Exp])
    (tyappE [tyfun : Exp]
            [tyarg : Type]))
  code:blank
  (define-type Value
    ....
    (polyV [body : Exp]
           [env : Env]))
  code:blank
  (define-type Type
    ....
    (idT [n : Symbol])
    (forallT [n : Symbol]
             [body : Type]))))


(define-syntax-rule (ex e ...)
  (list (para (code e ...))))

(slide
 #:title "Examples"
 'alts
 (list
  (ex (test (interp (parse `[LAMBDA ['a]
                              {lambda {[x : 'a]} x}])
                    mt-env)
            (polyV (lamE 'x (idT 'a) (idE 'x))
                   mt-env)))
  (ex (test (interp (parse `[@ [LAMBDA ['a]
                                 {lambda {[x : 'a]} x}]
                               num])
                    mt-env)
            (closV 'x (idE 'x) mt-env)))
  (ex (test (interp (parse `{[@ [LAMBDA ['a]
                                  {lambda {[x : 'a]} x}]
                                num]
                             8})
                    mt-env)
            (numV 8)))
  (ex (test (parse-type `(forall ('a) ('a -> 'a)))
            (forallT 'a (arrowT (idT 'a) (idT 'a)))))
  (ex (test (typecheck (parse `[LAMBDA ['a]
                                 {lambda {[x : 'a]} x}])
                       mt-env)
            (forallT 'a (arrowT (idT 'a) (idT 'a)))))
  (ex (test/exn (typecheck (parse `{lambda {[x : 'a]} x})
                           mt-env)
                "no type"))
  (ex (test (typecheck (parse `{lambda {[x : 'a]} x})
                       (extend-env
                        (tid 'a)
                        mt-env))
            (arrowT (idT 'a) (idT 'a))))
  (ex (test (typecheck (parse `[LAMBDA ['a]
                                 {lambda {[x : 'a]} x}])
                       mt-env)
            (forallT 'a (arrowT (idT 'a) (idT 'a)))))
  (ex (test (typecheck (parse `[@ [LAMBDA ['a]
                                    {lambda {[x : 'a]} x}]
                                  num])
                       mt-env)
            (arrowT (numT) (numT))))))

;; ----------------------------------------
(part 3)

(slide
 #:title "Type Checking"
 (infer (code #,(E) #,(ts) [LAMBDA [#,(A)] #,(M)] : #,(forall (A) (T)))
        (code #,(E+ (E) (inenv (A))) #,(ts) #,(M) : #,(T)))
 (blank)
 (infer (code #,(E) #,(ts) [@ #,(M) #,(T 0)] : #,(tsubs (T 1) (A) (T 0)))
        (ante-append
         (code #,(E) #,(ts) #,(T 0))
         (code #,(E) #,(ts) #,(M) : #,(forall (A) (T 1)))))
 (blank)
 (code #,(inenv (hbl-append (t "...") (A) (t "..."))) #,(ts) #,(A))
 (blank)
 (infer (code #,(E) #,(ts) #,(forall (A) (T)))
        (code #,(E+ (E) (inenv (A))) #,(ts) #,(T))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 4)

(define poly+tydef-title "Polymorphism and Type Definitions")

(slide
 #:title poly+tydef-title
 (para 
  #:width WWIDE
  "If we mix" (code LAMBDA) "with" (code let-type) ", then we can write")
 (scale
  (sscode
   {let {[f : (forall ('a) ('a -> num))
            [LAMBDA ['a]
              {lambda {[v : 'a]}
                {let-type [List {empty num}
                                {cons ('a * List)}]
                  {letrec {[len : (List -> num)
                                {lambda {[l : List]}
                                  {type-case List l
                                    [{empty n} 0]
                                    [{cons p} {+ 1 {len {snd p}}}]}}]}
                    {len {cons {pair v 
                                     {cons {pair v 
                                                 {empty 0}}}}}}}}}]]}
     {+ {[@ f num] 10}
        {[@ f (num -> num)] {lambda {[x : num]} x}}}})
  0.95)
 (para #:width WWIDE
       "This is a kind of polymorphic list definition")
 'next
 (para (bt "Problem:") "everything must be under a" (code LAMBDA)))

(slide
 #:title poly+tydef-title
 (para 
  #:width WWIDE
  (bt "Solution:") "build" (hbl-append (code LAMBDA) (t "-like"))
  "abstraction into" (code let-type))
 (blank)
 'alts
 (list
  (list
   (sscode
    {let-type [(Listof 'a) {empty num}
                           {cons ('a * (Listof 'a))}]
      {letrec {[len : ((List num) -> num)
                    {lambda {[l : (Listof num)]}
                      {type-case (Listof num) l
                        [{empty n} 0]
                        [{cons p} {+ 1 {len {snd p}}}]}}]}
        {len {[@ cons num] {pair 1 {[@ empty num] 0}}}}}}))
  (list
   (sscode
    {let-type [(Listof 'a) {empty num}
                           {cons ('a * (Listof 'a))}]
      {letrec {[len : (forall ('a) ((Listof 'a) -> num))
                    [LAMBDA ['a]
                      {lambda {[l : (Listof 'a)]}
                        {type-case (Listof 'a) l
                          [{empty n} 0]
                          [{cons p} {+ 1 {len {snd p}}}]}}]]}
        {+ {[@ len num] {[@ cons num] {pair 1 {[@ empty num] 0}}}}
           {[@ len (num -> num)] {[@ empty (num -> num)] 0}}}}}))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 5)

(define poly+inf-title "Polymorphism and Inference")

(slide
 #:title poly+inf-title
 (para #:width WWIDE
       "With polymorphism, type inference is usually combined"
       "with type-application inference:")
 (blank)
 (scode
  {let {[ident {lambda {[x : 'a]}
                 x}]}
    {ident {lambda {[y : num]} y}}}
  code:blank
  ⇒
  code:blank
  {let {[ident : (forall ('a) ('a -> 'a))
               [LAMBDA ['a]
                 {lambda {[x : 'a]}
                   x}]]}
    {[@ ident (num -> num)] {lambda {[y : num]} y}}}))

(slide
 #:title poly+inf-title
 (para #:width WWIDE "The" (code @) "part is easy:")
 (scode
  {let {[ident : (forall ('a) ('a -> 'a))
               [LAMBDA ['a]
                 {lambda {[x : 'a]}
                   x}]]}
    {ident {lambda {[y : num]} y}}})
 'next
 (para #:width WWIDE
       "The type application" (code [@ f (num -> num)])
       "is obvious, since we can get the type of"
       (code {lambda {[y : num]} y})))

(slide
 #:title poly+inf-title
 (code
  {let {[ident : ?
               {lambda {[x : ?]}
                 x}]}
    {ident {lambda {[y : num]} {ident 10}}}})
 (blank)
 (para "How about inferring a" (code LAMBDA)
       "around the value of" (code f) "?")
 'next
 (blank)
 (para #:align 'right "Yes, with some caveats..."))

(slide
 #:title poly+inf-title
 (para #:width WWIDE
       "Does the following expression have a type?")
 (code {lambda {[x : ?]} {x x}})
 'next
 (blank)
 (para #:width WWIDE
       "Yes, if we infer" (code forall) "types and type applications:")
 (code {lambda {[x : (forall ('a) ('a -> 'a))]}
         {[@ x (num -> num)] [@ x num]}})
 'next
 (blank)
 (blank)
 (para #:width WWIDE
       "Inferring types like this is arbitrarily difficult"
       "(i.e., undecidable), so type systems generally don't"))

(slide
 #:title "Let-Based Polymorphism"
 (para #:width WWIDE
       "Inference constraint: only infer a polymorphic type"
       "(and insert" (code LAMBDA) ") for ther right-hand side"
       "of a" (code let) "or" (code letrec) "binding")
 (item "This works:")
 (code {let {[ident : ?
                    {lambda {[x : ?]}
                      x}]}
         {ident {lambda {[y : num]} {ident 10}}}})
 'next
 (item "This doesn't:")
 (code {lambda {[x : ?]} {x x}})
 'next
 (blank)
 (para #:width WWIDE
       (bt "Note:") "makes" (code let) "a core form")
 (para #:width WWIDE
       (bt "Implementation:") "check right-hand side, add a"
       (code forall) "and" (code LAMBDA) "for each" "unconstrained" (it "new")
       "type variable"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Polymorphism and Inference and Type Definitions"
 (para #:width WWIDE
       "All three together make a practical programming system:")
 (blank)
 (sscode
  {let-type [(Listof 'a) {empty num}
                         {cons ('a * (Listof 'a))}]
    {letrec {[len : ?
                  {lambda {[l : (Listof 'a)]}
                    {type-case (Listof 'a) l
                      [{empty n} 0]
                      [{cons p} {+ 1 {len {snd p}}}]}}]}
      {+ {len {cons {pair 1 {empty 0}}}}
         {len {cons {pair {lambda {[x : num]} x} {empty 0}}}}}}}))

;; ------------------------------------------------------------
(part 6)

(define poly-values-title "Polymorphism and Values")

(slide
 #:title "Polymorphic Types"
 (code (lambda (x) x))
 'next
 (blank)
 (blank)
 (para "Why does Plait show")
 (code ('a -> 'a))
 (para "instead of")
 (code (forall ('a) ('a -> 'a)))
 (para "?"))

(parameterize ([current-para-width WWIDE])
  (slide
   #:title poly-values-title
   (para "A" (dt "polymorphic function") "is not quite a function")
   'next
   'alts
   (list
    (list
     (item "A" (bt "function") "is applied to a value to get a new value")
     (code f : (num -> num)
           {f 1})
     'next
     (item "A" (bt "polymorphic function") "is applied to a type to get a function")
     (code pf : (forall ('a) ('a -> 'a))
           {[@ pf num] 1}
           code:blank
           {pf 1} #,(colorize (bt "no type") "red")))
    (list))
   'alts
   (list
    (list
     (para "What happens if you write the following?")
     (scode {let {[f : ? {lambda {[v : ?]}
                           {lambda {[g : ?]} 
                             {g v}}}]}
              {let {[g : ? {lambda {[x : ?]} x}]}
                {{f 10} g}}})
     'next
     (blank)
     (para "A type application must be used at the function call, not in" (code f) ":")
     (scode {{[@ [@ f num] num] 10} [@ g num]}))
    (list
     (para "What happens if you write the following?")
     (scode {let {[f : ? {lambda {[v : ?]}
                           {lambda {[g : (forall ('a) ('a -> 'a))]}
                             {g v}}}]}
              {let {[g : ? {lambda {[x : ?]} x}]}
                {{f 10} g}}})
     'next
     (blank)
     (para "One type application must be used inside" (code f) ":")
     (scode [LAMBDA {'b} {lambda {[v : 'b]}
                           {lambda {[g : (forall ('a) ('a -> 'a))]}
                             {[@ g 'b] v}}}])))))

(parameterize ([current-para-width WWIDE])
  (slide
   #:title poly-values-title
   'alts
   (list
    (list
     (para "An argument that is a polymorphic value can be used in multiple ways:")
     (code {lambda {[g : (forall ('a) ('a -> 'a))]}
             {if {g #f}
                 {g 0}
                 {g 1}}})
     'next
     (para "but due to inference constraints,")
     (code {lambda {[g : ?]}
             {if {g #f}
                 {g 0}
                 {g 1}}})
     (para "would be rejected!"))
    (list
     (para "Plait (like ML) prohibits polymorphic values, so that")
     (code {lambda {[g : (forall ('a) ('a -> 'a))]}
             {if {g #f}
                 {g 0}
                 {g 1}}})
     (para "is not allowed")
     'next
     (item "Consistent with inference")
     'next
     (vc-append
      (/ gap-size 2)
      (item "Every" (code forall) "appears at the beginning of a type, so")
      (code (forall ('a) (forall ('b) ('a -> 'b))))
      (item #:bullet (ghost bullet) "can be abbreviated")
      (code ('a -> 'b))
      (item #:bullet (ghost bullet) "without loss of information"))))))
