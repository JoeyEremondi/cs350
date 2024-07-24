#lang slideshow
(require scheme/class
         scheme/gui/base
         slideshow/code
         "utils/utils.ss" 
         "utils/alg.ss"
         "utils/colors.ss"
         "utils/posn3d-obj.ss"
         "utils/classbox.ss")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 1)

(slide
 #:title "Classes"
 (scode {class (code:line Posn extends Object)
          {x y}
          [mdist {arg} {+ {get this x} {get this y}}]
          [addDist {arg} {+ {send arg mdist 0}
                            {send this mdist 0}}]}
         code:blank
         {class (code:line Posn3D extends Posn)
           {z}
           [mdist {arg} {+ {get this z} 
                           {super mdist arg}}]}
         code:blank
         {send {new Posn3D 1 2 3} addDist {new Posn 3 4}}))

(slide
 #:title "Typechecking Programs with Classes"
 (para "A well-formed program should never error with")
 'next
 (item "not a number")
 'alts
 (list (list (code {+ 1 {new Posn 1 2}}))
       null)
 (item "not an object")
 'alts
 (list (list (code {send 1 mdist 0}))
       (list (code {get 1 x}))
       null)
 (item "wrong field count")
 'alts
 (list (list (code {new Posn3D 1 2}))
       null)
 (vl-append
  (current-line-sep)
  (item "not found")
  (subitem "class, field, or method"))
 'alts
 (list (list (code {new SquareCircle}))
       (list (code {get {new Posn 1 2} z}))
       (list (code {send {new Posn 1 2} area}))
       (list (code {class (code:line Circle extends Object)
                     {}
                     [area {arg} {super area arg}]}))))

(slide
 #:title "Typed Class Language"
 (scale/improve-new-text
  (grammar-table
   (list (nonterm "Class") eqls (code {class (code:line #,(nonterm "Symbol") extends #,(nonterm "Symbol"))
                                        {#,(repeat (nonterm "Field"))} 
                                        #,(repeat (nonterm "Method"))}) 
         (blank)
         (nonterm "Field") eqls (code [#,(nonterm "Symbol") : #,(nonterm "Type")]) new-label
         (nonterm "Method") eqls (code [#,(nonterm "Symbol") {[arg : #,(nonterm "Type")]} : #,(nonterm "Type") #,(nonterm "Expr")]) new-label
         (nonterm "Type") eqls (code num) new-label
         (blank)          -or- (nonterm "Symbol") new-label))
  0.85))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 2)

(define NO (para #:fill? #f (colorize (bt "No") RedColor) sym:emdash))
(define YES (colorize (bt "Yes") GreenColor))

(define (should-check prog result)
  (list
   (para "Is this program well-formed?")
   prog
   'next
   (blank)
   result))

(slide
 #:title "Typechecking Programs with Classes"
 'alts
 (list
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {send {get this x} mdist 0}
                {send {get this y} mdist 0}}]}
    code:blank
    10)
   (para NO "the" (code x) "and" (code y) "fields are not objects"))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this z}}]}
    code:blank
    10)
   (para NO (code Posn) "has no" (code z) "field"))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {send this get-y 0}}]}
    code:blank
    10)
   (para NO (code Posn) "has no" (code get-y) "method"))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : Posn
             {+ {get this x} {get this y}}]}
    code:blank
    10)
   (para NO "result type for" (code mdist)
         "does not match body type"))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]}
    code:blank
    10)
   (para YES))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]}
    code:blank
    {new Posn 12})
   (para NO "wrong number of fields in" (code new)))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]}
    code:blank
    {new Posn 12 {new Posn 1 2}})
   (para NO "wrong field type for first" (code new)))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]
      [clone {[arg : num]} : Posn
             {new Posn {get this x} {get this y}}]}
    code:blank
    {send {new Posn 1 2} clone 0})
   (para YES))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]
      [clone {[arg : num]} : Posn
             {new Posn {get this x} {get this y}}]}
    code:blank
    {class (code:line Posn3D extends Posn)
      {[z : num]}
      [mdist {[arg : num]} : num
             {+ {get this z} {super mdist arg}}]}
    code:blank
    {new Posn3D 5 7 3})
   (para YES))
  
  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]
      [clone {[arg : num]} : Posn
             {new Posn {get this x} {get this y}}]}
    code:blank
    {class (code:line Posn3D extends Posn)
      {[z : num]}
      [mdist {[arg : num]} : Posn
             {new Posn 10 10}]}
    code:blank
    {new Posn3D 5 7 3})
   (para NO "override of" (code mdist) "changes result type"))

  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]
      [clone {[arg : num]} : Posn
             {new Posn {get this x} {get this y}}]}
    code:blank
    {class (code:line Posn3D extends Posn)
      {[z : num]}
      [mdist {[arg : num]} : num
             {+ {get this z} {super mdist arg}}]
      [clone {[arg : num]} : num
             10]}
    code:blank
    {new Posn3D 5 7 3})
   (para NO "override of" (code clone) "changes result type"))

  (should-check 
   (sscode
    {class (code:line Posn extends Object)
      {[x : num] [y : num]}
      [mdist {[arg : num]} : num
             {+ {get this x} {get this y}}]
      [clone {[arg : num]} : Posn
             {new Posn {get this x} {get this y}}]}
    code:blank
    {class (code:line Posn3D extends Posn)
      {[z : num]}
      [mdist {[arg : num]} : num
             {+ {get this z} {super mdist arg}}]
      [clone {[arg : num]} : Posn
             {new Posn3D {get this x} {get this y}
                  {get this z}}]}
    code:blank
    {new Posn3D 5 7 3})
   (para YES sym:emdash "which means that we need subtypes"))))

(slide
 #:title "Typechecking Summary"
 (item "Use class names as type")
 (item "Check for field and method existence")
 (item "Check field, method, and argument types")
 (item "Check fields against" (code new))
 (item "Check consistency of overrides")
 (item "Treat subclasses as subtypes"))
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 3)

(slide
 #:title "Datatypes"
 (scode
  (define-type ClassT
    (classT [super-name : Symbol]
            [fields : (Listof (Symbol * Type))]
            [methods : (Listof (Symbol * MethodT))]))
  code:blank
  (define-type MethodT
    (methodT [arg-type : Type]
             [result-type : Type]
             [body-expr : ExpI]))))

(slide
 #:title "Datatypes"
 (scode
  (define-type Type
    (numT)
    (objT [class-name : Symbol]))))

(slide
 #:title "Type Checking"
 (scode
  (define (typecheck [a : ExpI]
                     [t-classes : (Listof (Symbol * ClassT))]) : Type
    (begin
      (map (lambda (tc)
             (typecheck-class (fst tc) (snd tc) t-classes))
           t-classes)
      (typecheck-expr a t-classes (objT 'Object) (numT))))))

(slide
 #:title "Type Checking: Classes"
 (scode
  (define (typecheck-class [class-name : Symbol]
                           [t-class : ClassT]
                           [t-classes : (Listof (Symbol * ClassT))])
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (map (lambda (m)
              (begin
                (typecheck-method (snd m) (objT class-name) t-classes)
                (check-override (fst m) (snd m) t-class t-classes)))
            methods)]))))

(slide
 #:title "Type Checking: Methods"
 (scode
  (define (typecheck-method [method : MethodT]
                            [this-type : Type]
                            [t-classes : (Listof (Symbol * ClassT))]) : ()
    (type-case MethodT method
      [(methodT arg-type result-type body-expr)
       (if (is-subtype? (typecheck-expr body-expr t-classes
                                        this-type arg-type)
                        result-type
                        t-classes)
           (values)
           (type-error body-expr (to-string result-type)))]))))

(slide
 #:title "Type Checking: Method Overrides"
 (sscode
  (define (check-override [method-name : Symbol]
                          [method : MethodT]
                          [this-class : ClassT]
                          [t-classes : (Listof (Symbol * ClassT))])
    (local [(define super-name 
              (classT-super-name this-class))
            (define super-method
              (try
               ;; Look for method in superclass:
               (find-method-in-tree method-name
                                    super-name
                                    t-classes)
               ;; no such method in superclass:
               (lambda () method)))]
      (if (and (equal? (methodT-arg-type method)
                       (methodT-arg-type super-method))
               (equal? (methodT-result-type method)
                       (methodT-result-type super-method)))
          (values)
          (error 'typecheck (string-append
                             "bad override of "
                             (to-string method-name))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 4)

(define (typecheck-slide middle keep?
                         #:extra-defn [extra-defn (code ....)])
  (if (or keep? (not condense?))
      (slide
       #:title "Type Checking Expressions"
       #:layout 'top
       (para
        #:width (get-client-w #:aspect 'fullscreen)
        (sscode
         (define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type -> Type)
           (lambda (expr t-classes this-type arg-type)
             (local [(define (recur expr)
                       (typecheck-expr expr t-classes this-type arg-type))
                     #,extra-defn]
               (type-case ExpI expr
                 ....
                 #,(un-ss-scale middle)
                 ....)))))))
      (skip-slides 1)))

(typecheck-slide
 (sscode
  [(numI n) (numT)]
  ....
  [(argI) arg-type]
  [(thisI) this-type])
 #t)

(typecheck-slide
 (sscode
  [(plusI l r) (typecheck-nums l r)]
  [(multI l r) (typecheck-nums l r)])
 #:extra-defn
 (code
  (define (typecheck-nums l r)
    (type-case Type (recur l)
      [(numT )
       (type-case Type (recur r)
         [(numT) (numT)]
         [else (type-error r "num")])]
      [else (type-error l "num")])))
 #t)

(define placeholder (sscode ...))

(define (typecheck-steps . steps)
  (let loop ([steps (cdr steps)][accum (car steps)])
    (if (null? steps)
        (typecheck-slide (accum (blank)) #t)
        (if condense?
            (loop (cdr steps) (lambda (e) (accum ((car steps) e))))
            (begin
              (typecheck-slide (accum placeholder) #f)
              (loop (cdr steps)
                    (lambda (e) (accum ((car steps) e)))))))))

(typecheck-steps
 (lambda (x)
   (sscode
    [(newI class-name exprs)
     #,(un-ss-scale x)]))
 (lambda (x)
   (sscode
    (local [(define arg-types (map recur exprs))
            (define field-types
              (get-all-field-types class-name t-classes))]
      #,(un-ss-scale x))))
 (lambda (x)
   (sscode
    (if (and (= (length arg-types) (length field-types))
             (foldl (lambda (b r) (and r b))
                    #t
                    (map2 (lambda (t1 t2) 
                            (is-subtype? t1 t2 t-classes))
                          arg-types
                          field-types)))
        #,(un-ss-scale x)
        (type-error expr "field type mismatch"))))
 (lambda (x)
   (sscode (objT class-name))))

(typecheck-steps
 (lambda (x)
   (sscode
    [(getI obj-expr field-name)
     #,(un-ss-scale x)]))
 (lambda (x)
   (sscode
    (type-case Type (recur obj-expr)
      [(objT class-name)
       #,(un-ss-scale x)]
      [else (type-error obj-expr "object")])))
 (lambda (x)
   (sscode
    (find-field-in-tree field-name
                        class-name
                        t-classes))))

(typecheck-steps
 (lambda (x)
   (sscode
    [(sendI obj-expr method-name arg-expr)
     #,(un-ss-scale x)]))
 (lambda (x)
   (sscode
    (local [(define obj-type (recur obj-expr))
            (define arg-type (recur arg-expr))]
      #,(un-ss-scale x))))
 (lambda (x)
   (sscode
    (type-case Type obj-type
      [(objT class-name)
       #,(un-ss-scale x)]
      [else
       (type-error obj-expr "object")])))
 (lambda (x)
   (sscode
    (typecheck-send class-name method-name
                    arg-expr arg-type
                    t-classes))))

(typecheck-steps
 (lambda (x)
   (sscode
    [(superI method-name arg-expr)
     #,(un-ss-scale x)]))
 (lambda (x)
   (sscode
    (local [(define arg-type (recur arg-expr))
            (define this-class
              (find t-classes (objT-class-name this-type)))]
      #,(un-ss-scale x))))
 (lambda (x)
   (sscode
    (typecheck-send (classT-super-name this-class)
                    method-name
                    arg-expr arg-type
                    t-classes))))

(slide
 #:title "Type Checker: Sends"
 (sscode
  (define (typecheck-send [class-name : Symbol]
                          [method-name : Symbol]
                          [arg-expr : ExpI]
                          [arg-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))])
    (type-case MethodT (find-method-in-tree
                        method-name
                        class-name
                        t-classes)
      [(methodT arg-type-m result-type body-expr)
       (if (is-subtype? arg-type arg-type-m t-classes)
           result-type
           (type-error arg-expr (to-string arg-type-m)))]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 5)

(define subtype-title "Type Checker: Subtypes")

(define ticae-posn
  (ssscode
   {class (code:line Posn extends Object)
     {[x : num] [y : num]}
     [mdist {[arg : num]} : num
            {+ {get this x} {get this y}}]
     [addDist {[arg : Posn]} : num
              {+ {send this mdist 0} {send arg mdist 0}}]}))

(define ticae-posn3d
  (ssscode {class (code:line Posn3D extends Posn)
             {[z : num]}
             [mdist {[arg : num]} : num
                    {+ {get this z} {super mdist arg}}]}
           {send {new Posn3D 7 5 3} mdist 0}))

(slide
 #:title subtype-title
 (scode
  #,(un-sss-scale ticae-posn)
  code:blank
  #,(un-sss-scale ticae-posn3d)))

(define (subtype-slide #:top [top (blank)]
                       #:c-obj [c-obj (code Object)]
                       #:c-posn [c-posn (code Posn)]
                       #:c-posn3d [c-posn3d (code Posn3D)]
                       #:c-animal [c-animal (code Animal)]
                       #:c-snake [c-snake (code Snake)]
                       #:c-tiger [c-tiger (code Tiger)]
                       #:next [next #f])
  (slide
   #:title subtype-title
   (vc-append
    gap-size
    (let ([obj (classbox c-obj null null)]
          [posn (classbox c-posn null null)]
          [posn3d (classbox c-posn3d null null)]
          [animal (classbox c-animal null null)]
          [snake (classbox c-snake null null)]
          [tiger (classbox c-tiger null null)]
          [vs (* 3 gap-size)])
      (let* ([a (add-subclass-lines
                 (vc-append vs
                            animal
                            (ht-append gap-size snake tiger))
                 animal (list snake tiger))]
             [p (add-subclass-lines
                 (vc-append vs posn posn3d)
                 posn (list posn3d))]
             [q (vc-append vs
                           obj
                           (ht-append (* 5 gap-size)
                                      p
                                      a))])
        (let ([p (add-subclass-lines q obj (list posn animal))])
          (refocus (vc-append (* 3 gap-size) top p)
                   p)))))
   (if next 'next 'nothing)
   (blank)
   (let ([p (blank)])
     (refocus (ct-superimpose p 
                              (or next (blank)))
              p))))

(define-syntax-rule (code/1 s)
  (colorize (parameterize ([code-colorize-enabled #f])
              (code s))
            "red"))

(define-syntax-rule (code/2 s)
  (colorize (parameterize ([code-colorize-enabled #f])
              (code s))
            "darkred"))

(define (is-subtype?-p a b)
  (para #:fill? #f a "is a subtype of" b "?"))

(subtype-slide)
(subtype-slide #:top (is-subtype?-p (code/1 Posn) (code/2 Object))
               #:c-posn (code/1 Posn)
               #:c-obj (code/2 Object)
               #:next (para #:fill? #f YES "--- starting from" (code/1 Posn) "reaches" (code/2 Object)))
(subtype-slide #:top (is-subtype?-p (code/1 Object) (code/2 Posn))
               #:c-posn (code/2 Posn)
               #:c-obj (code/1 Object)
               #:next (para #:fill? #f NO "starting from" (code/1 Object) "doesn't reach" (code/2 Posn)))
(subtype-slide #:top (is-subtype?-p (code/2 Object) (code/2 Object))
               #:c-obj (code/2 Object)
               #:next (para #:fill? #f YES "--- match at start"))
(subtype-slide #:top (is-subtype?-p (code/2 Posn) (code/2 Posn))
               #:c-posn (code/2 Posn)
               #:next (para #:fill? #f YES "--- match at start"))
(subtype-slide #:top (is-subtype?-p (code/1 Tiger) (code/2 Posn))
               #:c-tiger (code/1 Tiger)
               #:c-posn (code/2 Posn)
               #:next (para #:fill? #f NO "starting from" (code/1 Tiger) "doesn't reach" (code/2 Posn)))

(slide
 #:title subtype-title
 (sscode
  (define (is-subclass? name1 name2 t-classes)
    (cond
      [(equal? name1 name2) #t]
      [(equal? name1 'Object) #f]
      [else
       (type-case ClassT (find t-classes name1)
         [(classT super-name fields methods)
          (is-subclass? super-name name2 t-classes)])]))
  code:blank
  (define (is-subtype? t1 t2 t-classes)
    (type-case Type t1
      [(objT name1)
       (type-case Type t2 
         [(objT name2)
          (is-subclass? name1 name2 t-classes)]
         [else #f])]
      [else (equal? t1 t2)]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 6)

(define (lang name features example)
  (list
   (frame (inset
           (cc-superimpose
            (blank (pict-height example) 0)
            (vc-append
             gap-size 
             (tt name)
             (apply
              vc-append
              (* 2 (current-line-sep))
              (map (lambda (s) (if (string? s) (t s) s)) features))))
           (* 2 (current-line-sep))))
   example))

(define down-arrow (colorize (arrow gap-size (* pi 3/2))
                             GreenColor))

(slide
 #:title "Implementing Classes"
 #:layout 'tall
 (table
  2
  (append
   (lang "ClassT"
         `("types"
           "")
         (vl-append
          (current-line-sep)
          ticae-posn
          ticae-posn3d))
   (list down-arrow (blank))
   (lang "ClassI"
         `("inheritance"
           "super")
         (ssscode {class (code:line Posn extends Object)
                    {x y}
                    [mdist {arg} {+ {get this x} {get this y}}]
                    [addDist {arg} {+ {send this mdist 0} {send arg mdist 0}}]}
                  {class (code:line Posn3D extends Posn)
                    {z}
                    [mdist {arg} {+ {get this z} {super mdist arg}}]}
                  {send {new Posn3D 7 5 3} mdist 0}))
   (list down-arrow (blank))
   (lang "Class"
         `("method dispatch"
           "fields")
         (ssscode {class Posn
                    {x y}
                    [mdist {arg} {+ {get this x} {get this y}}]
                    [addDist {arg} {+ {dsend this mdist 0} {dsend arg mdist 0}}]}
                  {class Posn3D
                    {x y z}
                    [mdist {arg} {+ {get this z} {ssend this Posn mdist arg}}]
                    [addDist {arg} {+ {dsend this mdist 0} {dsend arg mdist 0}}]}
                  {dsend {new Posn3D 7 5 3} mdist 0})))
  (cons cc-superimpose lc-superimpose) cc-superimpose
  (if widescreen? (* 3 gap-size) gap-size) (* 0.5 gap-size)))

(slide
 #:title "Interpreter"
 (sscode
  (define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
    (lambda (a t-classes)
      (interp-i a
                (map (lambda (c)
                       (values (fst c) (strip-types (snd c))))
                     t-classes))))
  code:blank
  (define strip-types : (ClassT -> ClassI)
    (lambda (t-class)
      (type-case ClassT t-class
        [(classT super-name fields methods)
         (classI
          super-name
          (map fst fields)
          (map (lambda (m)
                 (values (fst m)
                         (type-case MethodT (snd m)
                           [(methodT arg-type result-type body-expr)
                            body-expr])))
               methods))])))))
