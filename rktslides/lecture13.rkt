#lang slideshow
(require racket/class
         slideshow/code
         slideshow/balloon
         "utils/utils.ss" 
         "utils/alg.ss"
         "utils/colors.ss"
         "utils/posn3d-obj.ss")

(define fp-title "Functional Programming")

(define WIDE (* (get-client-w #:aspect 'fullscreen) 0.9))

(define (java . l)
  (apply vl-append
         (current-line-sep)
         (map tt l)))

(define dynamic-color "blue")
(define static-color "purple")

(define java/hilite
  (case-lambda
   [(s) (cond
         [(regexp-match #rx"^(.*)_([^_]*)_(.*)$" s)
          => (lambda (m)
               (hbl-append
                (java/hilite (cadr m))
                (colorize (tt (caddr m)) dynamic-color)
                (java/hilite (cadddr m))))]
         [(regexp-match #rx"^(.*)%([^_]*)%(.*)$" s)
          => (lambda (m)
               (hbl-append
                (java/hilite (cadr m))
                (colorize (tt (caddr m)) static-color)
                (java/hilite (cadddr m))))]
         [else
          (java s)])]
   [l (apply
       vl-append
       (current-line-sep)
       (map java/hilite l))]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 1)

(slide
 #:title "Objects and Constructors"
 (scode
  (define (numV n)
    (list (values 'apply (lambda (arg-val) ....))
          (values 'number (lambda () ...))))
  code:blank
  (define (closV n body c-env)
    (list (values 'apply (lambda (arg-val) ....))
          (values 'number (lambda () ....)))))
 'next
 (blank)
 (item "result of" (code numV) "or" (code closV) "is an" (bit "object"))
 'next
 (item "functions for" (code 'apply) "and" (code 'number) "are" (bit "methods"))
 'next
 (item (code n) "," (code body) ", and" (code c-env) "are" (bit "fields"))
 'next
 (item (code numV) "and" (code closV) "themselves are" (bit "constructors"))
 'next
 (para #:align 'right "... and not far from" (bit "classes")))

(slide
 #:title "Classes"
 (para #:width WIDE "Classes play two (dynamic) roles:")
 (item #:aspect 'fullscreen "Object construction")
 (scale (java "class Snake {"
              "  ..."
              "}"
              " "
              "new Snake(\"Slinky\", 10);") 
        0.8)
 'next
 (item  #:aspect 'fullscreen"Implementation inheritance")
 (scale
  (java "class Rattlesnake extends Snake {"
        "  ..."
        "}")
  0.8)
 'next
 (subitem #:aspect 'fullscreen "Inheritance of methods")
 (subitem #:aspect 'fullscreen "Static method dispatch"))

(slide
 #:title "Classes: Static and Dynamic Dispatch"
 (ht-append
  gap-size
  (scale/improve-new-text
   (java/hilite
    "class Snake implements Animal {"
    "  ..."
    "  boolean endangers(Animal a) {"
    "    return (_a.slowerThan_(100)"
    "            && _a.isLighter_(this.weight/2));"
    "  }"
    "}"
    " "
    "class Rattlesnake extends Snake {"
    "   ..."
    "   boolean endangers(Animal a) {"
    "     return (!_a.hasThickSkin_()"
    "             || %super.endangers%(a))"
    "   }"
    "}"
    " "
    "Animal a = new Rattlesnake(...);"
    "Animal b = new Mouse(...);"
    " "
    "_a.endangers_(b);")
   0.75)
  (frame
   (inset
    (vc-append
     (* 2 (current-line-sep))
     (colorize (bt "dynamic") dynamic-color)
     (colorize (bt "static") static-color))
    (* 4 (current-line-sep))))))

;; ------------------------------------------------------------
(part 2)

(define (ajava #:scale [s 0.6] . l)
  (inset
   (let ([p (frame
             (inset
              (scale/improve-new-text 
               (vl-append
                (apply java l))
               s)
              (* 2 (current-line-sep))))])
     (refocus (vc-append
               (current-line-sep)
               (scale (t "Analogous Java code") 0.5)
               p)
              p))
   0 (- (* 1/2 gap-size)) (- (* gap-size 2)) 0))

(define (analogous-to n j)
  (ht-append gap-size n j))

(define field-expr (nonterm "Exp"))
(define method-expr (nonterm "Exp"))

(define (stack . l) (apply vl-append 10 l))

(define (iframe p)
  (hilite #:color "beige" p))

(slide
 #:title "Class Language with Explicit Static Calls"
 (scale/improve-new-text
  (hc-append
   (- (* 2 (pict-width new-label)))
   (iframe
    (grammar-table
     (list (nonterm "Class") eqls (code {class #,(nonterm "Symbol") 
                                          {#,(repeat (nonterm "Field"))} 
                                          #,(repeat (nonterm "Method"))}) 
           new-label
           (nonterm "Field") eqls (nonterm "Symbol") (blank)
           (nonterm "Method") eqls (code [#,(nonterm "Symbol") {arg} #,method-expr]) (blank))))
   (grammar-table
    (list (nonterm "Exp") eqls (nonterm "Number") (blank)
          (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code arg) (blank)
          (blank)         -or- (code this) (blank)
          (blank)         -or- (code {new #,(nonterm "Symbol") #,(repeat (nonterm "Exp"))}) new-label
          (blank)         -or- (code {get #,(nonterm "Exp") #,(nonterm "Symbol")}) (blank)
          (blank)         -or- (code {send #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code {ssend #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Symbol") #,(nonterm "Exp")}) new-label)))
  0.7)
 'next
 (blank)
 'alts
 (list (list
        (blank)
        (blank)
        (analogous-to
         (scale
          (stack
           (sshcode {class Posn
                      {x y}
                      [mdist {arg} {+ {get this x} {get this y}}]
                      [addDist {arg} {+ {send arg mdist 0}
                                        {send this mdist 0}}]})
           (sshcode {send {new Posn 1 2} 
                          addDist
                          {new Posn 3 4}}))
          1.2)
         (ajava "class Posn {"
                "  int x, y;"
                "  int mdist() {"
                "    return this.x + this.y;"
                "  }"
                "  int addDist(Posn p) {"
                "    return p.mdist() + mdist();"
                "  }"
                "}"
                "new Posn(1,2).mdist(new Posn(3,4))")))
       (list
        (analogous-to
         (stack
          (sshcode {class (code:line Posn ...)
                     [addDist {arg} {+ {send arg mdist 0}
                                       {send this mdist 0}}]})
          (sshcode {class Posn3D                    
                     {x y z}
                     [mdist {arg} {+ {get this z}
                                     {ssend this Posn mdist arg}}]
                     [addDist {arg} {ssend this Posn addDist arg}]})
          (sshcode {send {new Posn3D 1 2 3} 
                         addDist
                         {new Posn 3 4}}))
         (ajava #:scale 0.55
                "class Posn {"
                "  ... as before ..."
                "}"
                "class Posn3D extends Posn {"
                "  int z; ..."
                "  int mdist() {"
                "    return this.z + super.mdist();"
                "  }"
                "  int addDist(Posn p) {"
                "    return super.addDist(p);"
                "  }"
                "}"
                "new Posn3D(1,2,3).addDist(new Posn(3,4))")))))

;; ----------------------------------------
(part 3)

(slide
 #:title "Object Values"
 (para "How does")
 (code {send {new Posn3D 1 2 3} mdist ...})
 (para "dispatch to the right" (code mdist) "?")
 'next
 (blank)
 (para "The result of" (code {new Posn3D 1 2 3}) "can now"
       "hold a class tag and field values:")
 (posn3d-obj)
 (para "Look for field names and methods in the class"))

(slide
 #:title "Classes and Object Values"
 (code
  (define-type Value
    (numV [n : Number])
    (objV [class-name : Symbol]
          [field-values : (Listof Value)]))
  code:blank
  (define-type Class
    (classC [field-names : (Listof Symbol)]
            [methods : (Listof (Symbol * Exp))])))
 'next
 (blank)
 (blank)
 (code
  interp : (Exp (Listof (Symbol * Class)) Value Value 
                -> Value)))

(define posn-class
  (code
   (define posn-class
     (values 'Posn
             (classC (list 'x 'y)
                     (list
                      (values 'mdist
                              (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                      (values 'addDist
                              (plusE (sendE (thisE) 'mdist (numE 0))
                                     (sendE (argE) 'mdist (numE 0))))))))))
(define posn3d-class
  (code
   (define posn3D-class
     (values 'Posn3D
             (classC (list 'x 'y 'z)
                     (list 
                      (values 'mdist
                              (plusE (getE (thisE) 'z)
                                     (ssendE (thisE) 'Posn 'mdist (argE))))
                      (values 'addDist
                              (ssendE (thisE) 'Posn 'addDist (argE)))))))))

(define posn27-expr
  (code
   (define new-posn27
     (newE 'Posn (list (numE 2) (numE 7))))))

(define posn531-expr
  (code
   (define new-posn531
     (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))))

(slide
 #:title "Examples"
 #:layout 'tall
 'alts
 (list
  (list
   (code (test (interp (numE 10) 
                       empty
                       (objV 'Object empty)
                       (numV 0))
               (numV 10))))
  (list
   (sscode
    #,posn-class
    #,posn3d-class
    code:blank
    (define (interp-posn a)
      (interp a (list Posn-class Posn3D-class) (objV 'Object empty) (numV 0)))))
  (list
   (code (test (interp-posn (newE 'Posn 
                                  (list (numE 2)
                                        (numE 7))))
               (objV 'Posn
                     (list (numV 2)
                           (numV 7)))))
   'next
   (blank)
   (blank)
   (blank)
   (code #,posn27-expr))
  (list
   (scode #,(hilite
             #:color "lightgray"
             (code #,(scale posn-class 0.75)
                   code:blank
                   #,(scale posn27-expr 0.75)))
          code:blank
          code:blank
          (test (interp-posn (sendE new-posn27 'mdist (numE 0)))
                (numV 9))))
  (list
   (inset
    (scode #,(hilite
              #:color "lightgray"
              (code #,(scale posn-class 0.75)
                    #,(scale posn3d-class 0.75)
                    #,(scale posn27-expr 0.75)
                    #,(scale posn531-expr 0.75)))
           code:blank
           (test (interp-posn (sendE new-posn531 'addDist new-posn27))
                 (numV 18)))
    0 (- (* 2 gap-size)) 0 0))))

;; ----------------------------------------
(part 4)

(define (interp-slide keep? middle)
  (if (or keep? (not condense?))
      (slide
       #:title "Interpreter"
       #:layout 'top
       (para
        #:width (if widescreen? (* client-w 0.9) client-w)
        (xscode
         (define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
           (lambda (a classes this-val arg-val)
             (local [(define (recur expr)
                       (interp expr classes this-val arg-val))]
               (type-case Exp a
                 ...
                 #,(un-xs-scale middle)
                 ...)))))))
      (skip-slides 1)))

(interp-slide
 #f
 (blank))

;; --------------------

(interp-slide
 #t
 (xscode
  [(numE n) (numV n)]
  [(plusE l r) (num+ (recur l) (recur r))]
  [(multE l r) (num* (recur l) (recur r))]
  [(thisE) this-val]
  [(argE) arg-val]))


;; --------------------

(interp-slide
 #f
 (xscode
  [(newE class-name field-exprs)
   ...]))

(interp-slide
 #f
 (xscode
  [(newE class-name field-exprs)
   (local [(define c (find classes class-name))
           (define vals (map recur field-exprs))]
     ...)]))

(interp-slide
 #f
 (xscode
  [(newE class-name field-exprs)
   (local [(define c (find classes class-name))
           (define vals (map recur field-exprs))]
     ...
     (objV class-name vals)
     ...)]))

(interp-slide
 #t
 (xscode
  [(newE class-name field-exprs)
   (local [(define c (find classes class-name))
           (define vals (map recur field-exprs))]
     (if (= (length vals) (length (classC-field-names c)))
         (objV class-name vals)
         (error 'interp "wrong field count")))]))

;; --------------------

(interp-slide
 #f
 (xscode
  [(getE obj-expr field-name)
   ...]))

(interp-slide
 #f
 (xscode
  [(getE obj-expr field-name)
   (type-case Value (recur obj-expr)
     [(objV class-name field-vals)
      ....]
     [else (error 'interp "not an object")])]))

(interp-slide
 #f
 (xscode
  [(getE obj-expr field-name)
   (type-case Value (recur obj-expr)
     [(objV class-name field-vals)
      (type-case Class (find classes class-name)
        [(classC field-names methods)
         ....])]
     [else (error 'interp "not an object")])]))

(interp-slide
 #f
 (xscode
  [(getE obj-expr field-name)
   (type-case Value (recur obj-expr)
     [(objV class-name field-vals)
      (type-case Class (find classes class-name)
        [(classC field-names methods)
         .... field-names ....
         .... field-vals ....
         .... field-name ....])]
     [else (error 'interp "not an object")])]))

(interp-slide
 #t
 (xscode
  [(getE obj-expr field-name)
   (type-case Value (recur obj-expr)
     [(objV class-name field-vals)
      (type-case Class (find class-name classes)
        [(classC field-names methods)
         (find (map2 (lambda (n v) (values n v))
                     field-names
                     field-vals)
               field-name)])]
     [else (error 'interp "not an object")])]))

;; --------------------

(interp-slide
 #f
 (xscode
  [(sendE obj-expr method-name arg-expr)
   ...]))

(interp-slide
 #f
 (xscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj (recur obj-expr))
           (define arg-val (recur arg-expr))]
     ...)]))

(interp-slide
 #f
 (xscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj (recur obj-expr))
           (define arg-val (recur arg-expr))]
     (type-case Value obj
       [(objV class-name field-vals)
        ...]
       [else (error 'interp "not an object")]))]))

(interp-slide
 #t
 (xscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj (recur obj-expr))
           (define arg-val (recur arg-expr))]
     (type-case Value obj
       [(objV class-name field-vals)
        (call-method class-name method-name classes
                     obj arg-val)]
       [else (error 'interp "not an object")]))]))

;; --------------------

(define (align . l) (apply #:width (if widescreen? (* client-w 0.9) WIDE) para l))

(slide
 #:title "Calling a Method"
 #:layout 'top
 'alts~
 (list
  (list
   (align
    (xscode
     (define (call-method class-name method-name classes
                          obj arg-val)
       ...))))
  (list
   (align
    (xscode
     (define (call-method class-name method-name classes
                          obj arg-val)
       ... (find classes class-name) ...))))
  (list
   (align
    (xscode
     (define (call-method class-name method-name classes
                          obj arg-val)
       (type-case Class (find classes class-name)
         [(classC field-names methods)
          ...])))))
  (list
   (align
    (xscode
     (define (call-method class-name method-name classes
                          obj arg-val)
       (type-case Class (find classes class-name)
         [(classC field-names methods)
          ... (find methods method-name) ...])))))
  (list
   (align
    (xscode
     (define (call-method class-name method-name classes
                          obj arg-val)
       (type-case Class (find classes class-name)
         [(classC field-names methods)
          (let ([body-expr (find methods method-name)])
            ...)])))))
  (list
   (align
    (xscode
     (define (call-method class-name method-name classes
                          obj arg-val)
       (type-case Class (find classes class-name)
         [(classC field-names methods)
          (let ([body-expr (find methods method-name)])
            (interp body-expr
                    classes
                    obj
                    arg-val))])))))))

;; --------------------

(interp-slide
 #f
 (xscode
  [(ssendE obj-expr class-name method-name arg-expr)
   ...]))

(interp-slide
 #f
 (xscode
  [(ssendE obj-expr class-name method-name arg-expr)
   (local [(define obj (recur obj-expr))
           (define arg-val (recur arg-expr))]
     ...)]))

(interp-slide
 #t
 (xscode
  [(ssendE obj-expr class-name method-name arg-expr)
   (local [(define obj (recur obj-expr))
           (define arg-val (recur arg-expr))]
     (call-method class-name method-name classes
                  obj arg-val))]))

;; ----------------------------------------
(part 5)

(define c-example
  (sscode {class Posn
            {x y}
            [mdist {arg} {+ {get this x} {get this y}}]
            [addDist {arg} {+ {send arg mdist 0}
                              {send this mdist 0}}]}
          code:blank
          {class Posn3D
            {x y z}
            [mdist {arg} {+ {get this z} 
                            {ssend this Posn mdist arg}}]
            [addDist {arg} {+ {send arg mdist 0}
                              {send this mdist 0}}]}
          code:blank
          {send {new Posn3D 1 2 3} addDist {new Posn 3 4}}))

(define i-example
  (sscode {class (code:line Posn extends Object)
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
 #:title "Subclasses"
 (para "Subclasses with" (tt "Exp") ":")
 c-example
 'next
 (blank)
 (vc-append
  (current-line-sep)
  (para "Programmer manually")
  (item "duplicates fields")
  (item "implements method inheritance")))

(slide
 #:title "Subclasses"
 (para (tt "ExpI") "adds" (dt "implementation inheritance") ":")
 i-example)

(slide
 #:title "Class Language with Inheritance"
 (scale/improve-new-text
  (vl-append
   gap-size
   (grammar-table
    (list (nonterm "Class") eqls (code {class (code:line #,(nonterm "Symbol") extends #,(nonterm "Symbol"))
                                         {#,(repeat (nonterm "Field"))} 
                                         #,(repeat (nonterm "Method"))}) 
          new-label
          (nonterm "Field") eqls (nonterm "Symbol") (blank)
          (nonterm "Method") eqls (code [#,(nonterm "Symbol") {arg} #,method-expr]) (blank)))
   (grammar-table
    (list (nonterm "Exp") eqls (nonterm "Number") (blank)
          (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code arg) (blank)
          (blank)         -or- (code this) (blank)
          (blank)         -or- (code {new #,(nonterm "Symbol") #,(repeat (nonterm "Exp"))}) (blank)
          (blank)         -or- (code {get #,(nonterm "Exp") #,(nonterm "Symbol")}) (blank)
          (blank)         -or- (code {send #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Exp")}) (blank)
          (blank)         -or- (code {super #,(nonterm "Symbol") #,(nonterm "Exp")}) new-label)))
  0.7))

(slide
 #:title "Compiling Inheritance"
 (hc-append
  gap-size
  (scale i-example (if widescreen? 0.8 0.65))
  (colorize (arrow gap-size 0) "forestgreen")
  (scale c-example (if widescreen? 0.8 0.65)))
 'next
 (blank)
 (item #:aspect 'fullscreen "merge fields from superclasses")
 'next
 (item #:aspect 'fullscreen "change" (code super) "to" (code ssend))
 'next
 (item #:aspect 'fullscreen "merge/override methods"))

;; ----------------------------------------
(part 6)

(slide
 #:title "Classes"
 (scode
  (define-type ClassI
    (classI [super-name : Symbol]
            [field-names : (Listof Symbol)]
            [methods : (Listof (Symbol * ExpI))]))))

(slide
 #:title "Expressions"
 (scode
  (define-type ExpI
    (numI [n : Number])
    (plusI [lhs : ExpI]
           [rhs : ExpI])
    (multI [lhs : ExpI]
           [rhs : ExpI])
    (argI)
    (thisI)
    (newI [class-name : Symbol]
          [args : (Listof ExpI)])
    (getI [obj-expr : ExpI]
          [field-name : Symbol])
    (sendI [obj-expr : ExpI]
           [method-name : Symbol]
           [arg-expr : ExpI])
    (superI [method-name : Symbol]
            [arg-expr : ExpI]))))

(slide
 #:title "Examples"
 'alts
 (list
  (list
   (code (test (exp-i->c (numI 10))
               (numE 10))))
  (list
   (code (test (exp-i->c (thisI))
               (thisE))))
  (list
   (code (test (exp-i->c (superI 'mdist (numI 0)))
               (ssendE (thisE) #,(colorize (tt "???") "red") 'mdist (numE 0)))))
  (list
   (code exp-i->c : (ExpI Symbol -> Exp)
         code:blank
         (test (exp-i->c (superI 'mdist (numI 0)) 'Posn)
               (ssendE (thisE) 'Posn 'mdist (numE 0)))))))

(slide
 #:title "Compiling Expressions"
 (code
  (define (exp-i->c [a : ExpI] [super-name : Symbol]) : Exp
    (local [(define (recur expr)
              (exp-i->c expr super-name))]
      (type-case ExpI a
        [(numI n) (numE n)]
        [(plusI l r) (plusE (recur l) (recur r))]
        [(multI l r) (multE (recur l) (recur r))]
        ...
        [(superI method-name arg-expr)
         (ssendE (thisE)
                 super-name
                 method-name
                 (recur arg-expr))])))))

(slide
 #:title "Compiling Class Methods"
 (code
  (define (class-i->c-not-flat [c : ClassI]) : Class
    (type-case ClassI c
      [(classI super-name field-names methods)
       (classC
        field-names
        (map (lambda (m)
               (values (fst m)
                       (exp-i->c (snd m) super-name)))
             methods))]))))

(define (flatten-step inside)
  (list
   (para #:width (get-client-w #:aspect 'fullscreen)
         (xscode
          (define (flatten-class [name : Symbol]
                                 [classes-not-flat : (Listof (Symbol * Class))] 
                                 [i-classes : (Listof (Symbol * ClassI))]) : Class
            #,inside)))))
  
(define (flatten-super-step inside)
  (list
   (para #:width (get-client-w #:aspect 'fullscreen)
         (xscode
          (define (flatten-super [name : Symbol]
                                 [classes-not-flat : (Listof (Symbol * Class))] 
                                 [i-classes : (Listof (Symbol * ClassI))]) : Class
            #,inside)))))

(slide
 #:title "Flattening a Class"
 'alts
 (list
  (flatten-step
   (code ...))
  (flatten-step
   (code ...  (find classes-not-flat name) ...))
  (flatten-step
   (code
    (type-case Class (find classes-not-flat name)
      [(classC field-names methods)
       ...])))
  (flatten-step
   (code
    (type-case Class (find classes-not-flat name)
      [(classC field-names methods)
       ... (flatten-super name classes-not-flat i-classes) ...])))
  (flatten-step
   (code
    (type-case Class (find classes-not-flat name)
      [(classC field-names methods)
       (type-case Class (flatten-super name classes-not-flat i-classes)
         [(classC super-field-names super-methods)
          ...])])))
  (flatten-step
   (code
    (type-case Class (find classes-not-flat name)
      [(classC field-names methods)
       (type-case Class (flatten-super name classes-not-flat i-classes)
         [(classC super-field-names super-methods)
          (classC ....
                  ....)])])))
  (flatten-step
   (code
    (type-case Class (find classes-not-flat name)
      [(classC field-names methods)
       (type-case Class (flatten-super name classes-not-flat i-classes)
         [(classC super-field-names super-methods)
          (classC (add-fields super-field-names 
                              field-names)
                  ....)])])))
  (flatten-step
   (code
    (type-case Class (find classes-not-flat name)
      [(classC field-names methods)
       (type-case Class (flatten-super name classes-not-flat i-classes)
         [(classC super-field-names super-methods)
          (classC (add-fields super-field-names 
                              field-names)
                  (add/replace-methods super-methods 
                                       methods))])])))
  ;; ----------------------------------------
  (flatten-super-step
   (code ...))
  (flatten-super-step
   (code
    ... (find i-classes name) ...))
  (flatten-super-step
   (code
    (type-case ClassI (find i-classes name)
      [(classI super-name field-names i-methods)
       ...])))
  (flatten-super-step
   (code
    (type-case ClassI (find i-classes name)
      [(classI super-name field-names i-methods)
       ... (flatten-class super-name
                          classes-not-flat
                          i-classes) ...])))
  (flatten-super-step
   (code
    (type-case ClassI (find i-classes name)
      [(classI super-name field-names i-methods)
       (if (equal? super-name 'Object)
           (classC empty empty)
           (flatten-class super-name
                          classes-not-flat
                          i-classes))])))))

(slide
 #:title "Interpreter"
 (xscode
  (define (interp-i [i-a : ExpI]
                    [i-classes : (Listof (Symbol * ClassI))]) : Value
    (local [(define a (exp-i->c i-a 'Object))
            (define classes-not-flat
              (map (lambda (i)
                     (values (fst i)
                             (class-i->c-not-flat (snd i))))
                   i-classes))
            (define classes
              (map (lambda (c)
                     (let ([name (fst c)])
                       (values
                        name
                        (flatten-class name classes-not-flat i-classes))))
                   classes-not-flat))]
      (interp a classes (objV 'Object empty) (numV 0))))))
