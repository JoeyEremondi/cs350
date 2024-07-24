#lang slideshow
(require racket/class
         slideshow/code
         slideshow/balloon
         "utils/utils.ss" 
         "utils/alg.ss"
         "utils/colors.ss")

(define fp-title "Functional Programming")

(define WIDE (* (get-client-w #:aspect 'fullscreen) 0.9))

(define (java . l)
  (apply vl-append
         (current-line-sep)
         (map tt l)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 1)

(slide
 #:title fp-title
 #:layout 'top
 (para #:width WIDE
       (bit "Functional programming") "is avoiding state")
 'next
 (blank)
 (code
  (define (area [s : Shape]) : Number
    ....)
  (define (scale [s : Shape] [n : Number]) : Shape
    ....)
  code:blank
  (define r (rectangle 10 15))
  (test (area r)
        150)
  (test (area (scale r 2))
        600)
  (test (area r)
        150))
 'next
 (para #:align 'right "The alternative:" (bit "imperative programming")))

(slide
 #:title fp-title
 #:layout 'top
 (para #:width WIDE
       (bit "Functional programming") "often means using functions as values")
 'next
 (blank)
 (code (map (lambda (s) (> (area s) 40))
            (list (rectangle 10 5)
                  (square 6)
                  (equilateral-triangle 7))))
 
 'next
 (blank 0 (* gap-size 3))
 (para #:align 'right "The alternative:" (bit "first-order programming") "?"))

(define (make-code id)
  (code
   (define (#,id [s : Shape]) : Number
     (type-case Shape s
       [(rectangle w h) ...]
       [(square s) ...]
       [(equilateral-triangle s) ...]))))

(define area-code (make-code (code area)))
(define perimeter-code (make-code (code perimeter)))

(slide
 #:title fp-title
 #:layout 'top
 (para #:width WIDE
       (bit "Functional programming") "often means" (bit "datatype-oriented programming"))
 'next
 (blank)
 'alts
 (list
  (list
   (code (define-type Shape
           (rectangle [w : Number] [h : Number])
           (square [side : Number])
           (equilateral-triangle [side : Number]))
         code:blank
         #,area-code))
  (list
   (code
    (define (sum-of-areas [l : (Listof Shape)])
      (cond
       [(empty? l) 0]
       [(cons? l) (+ (area (first l))
                     (sum-of-areas (rest l)))])))))
 'next
 (blank 0 (* 2 gap-size))
 (para #:align 'right "The alternative:" (bit "object-oriented programming")))

(define (rev-vc-append sep a b)
  (pin-under (vc-append sep a (ghost b))
             b lt-find
             b))

(define vs-title "Datatype-Oriented versus Object-Oriented")

(define (vs-slide dop oop
                  #:oop-label [oop-label values])
  (slide
   #:title vs-title
   (rev-vc-append
    gap-size
    (dop
     widescreen?
     (para #:width (get-client-w #:aspect 'fullscreen)
           "Dataype-oriented: call an operation with a variant"))
    (dop
     (not widescreen?)
     (scale (hc-append (* 4 gap-size)
                       area-code
                       perimeter-code)
            0.6)))
   (blank)
   (rev-vc-append
    gap-size
    (oop
     widescreen?
     (oop-label
      (para #:width (get-client-w #:aspect 'fullscreen)
            "Object-oriented: call a variant with an operation")))
    (oop
     (not widescreen?)
     (scale (java "class Rectangle extends Shape { ..."
                  "  int area() { ... }"
                  "  int perimeter() { ... }"
                  "}"
                  "class Square extends Shape { ..."
                  "  int area() { ... }"
                  "  int perimeter() { ... }"
                  "}"
                  "class EquilateralTriangle extends Shape { ..."
                  "  int area() { ... }"
                  "  int perimeter() { ... }"
                  "}")
            0.6)))))

(define (make-add . l)
  (define p (apply vl-append (current-line-sep) l))
  (lambda (add? q)
    (if add?
        (let ([combine (if widescreen?
                           (lambda (a b)
                             (refocus (rt-superimpose (ctl-superimpose a (para #:width client-w " "))
                                                      b)
                                      a))
                           rb-superimpose)])
          (combine q (hilite #:color "lightblue" (inset p (/ gap-size 2) 0))))
        q)))

(define add-dop-note
  (make-add (item #:fill? #f "new" (bt "operation") "⇒" (colorize (t "new") "forestgreen") "function")
            (item #:fill? #f "new" (bt "variant") "⇒ " (colorize (t "change") "red") "functions")))
(define add-oop-note
  (make-add (item #:fill? #f "new" (bt "operation") "⇒" (colorize (t "change") "red") "objects")
            (item #:fill? #f "new" (bt "variant") "⇒ " (colorize (t "new") "forestgreen") "objects")))

(define (no-note add? p) p)

(vs-slide no-note (lambda (add? p) (ghost p)) #:oop-label ghost)
(vs-slide no-note no-note)
(vs-slide add-dop-note no-note)
(vs-slide add-dop-note add-oop-note)

;; ----------------------------------------
(part 2)

(slide
 #:title vs-title
 (vc-append
  (current-line-sep)
  (para #:align 'center "Functional programming can be")
  (para #:align 'center "datatype-oriented or object-oriented"))
 'next
 (blank)
 (para #:align 'center "We can use functions to represent objects..."))

(define obj-as-func-title "Representing Objects with Functions")

(define (align . l)
  (apply para #:width (* (get-client-w #:aspect 'fullscreen) 0.8) l))

(define (walign . l)
  (let ([p (apply para #:width (* (get-client-w) 0.9) l)])
    (if ((pict-width p) . > . client-w)
        (scale p (/ client-w (pict-width p)))
        p)))

(slide
 #:title obj-as-func-title
 (align
  (scode
   (define-type-alias Shape (-> Number))))
 'next
 (align
  (scode
   code:blank
   (define (rectangle w h) : Shape
     (lambda ()
       (* w h)))
   code:blank
   (define (square s) : Shape
     (lambda ()
       (* s s)))
   code:blank
   (define r (rectangle 10 15))
   (r) #,sym:implies 150))
 'next
 (align
  (scode
   code:blank
   (define c (let ([r 10])
               (lambda () (* pi (* r r)))))
   (c) #,sym:implies 314.179)))

;; ----------------------------------------
(part 5)

(slide
 #:title "Objects and Multiple Operations"
 (para #:width WIDE "Simple function implements an object with a single operation:")
 (code
  (define-type-alias Shape (-> Number))
  code:blank
  (define r (rectangle 10 15))
  (r))
 'next
 (blank)
 (para #:width WIDE "For multiple operations, could pass a symbol to select:")
 (code
  (define-type-alias Shape (Symbol -> Number))
  code:blank
  (define r (rectangle 10 15))
  (r 'area)
  (r 'perimeter)))

(slide
 #:title obj-as-func-title
 (align
  (scode
   (define-type-alias Shape (Symbol -> Number))))
 'next
 (align
  (scode
   (define (rectangle w h) : Shape
     (lambda (op)
       (cond
        [(equal? op 'area) (* w h)]
        [(equal? op 'perimeter) (* 2 (+ w h))])))
   code:blank
   (define (square s) : Shape
     (lambda (op)
       (cond
        [(equal? op 'area) (* s s)]
        [(equal? op 'perimeter) (* 4 s)])))
   code:blank
   (define r (rectangle 10 15))
   (r 'area) #,sym:implies 150
   (r 'perimeter) #,sym:implies 50)))

(slide
 #:title obj-as-func-title
 (walign
  (scode
   #,(tt "#lang plait #:untyped ...")
   code:blank
   (code:comment "A Shape is")
   (code:comment " (list (values 'area (-> Number))")
   (code:comment "       (values 'bigger-than? (Number -> Boolean))")
   (code:comment "       ....)")))
 'next
 'alts
 (list
  (list
   (walign
    (sscode
     (define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
       (type-case (Listof (Symbol * 'a)) l
         [empty
          (error 'find (string-append "not found: " (symbol->string name)))]
         [(cons p rst-l)
          (if (symbol=? (fst p) name)
              (snd p)
              (find rst-l name))])))))
  (list
   (walign
    (scode
     (define (rectangle w h)
       (list (values 'area (lambda () (* w h)))
             (values 'bigger-than? (lambda (n) (> (* w h) n)))))
     code:blank
     (define (square s)
       (list (values 'area (lambda () (* s s)))
             (values 'bigger-than? (lambda (n) (> (* s s) n)))))
     code:blank
     (define r (rectangle 10 15))
     ((find r 'area)) #,sym:implies 150
     ((find r 'bigger-than?) 100) #,sym:implies #t)))))

;; ----------------------------------------
(part 7)

(slide
 #:title "Objects without Functions"
 (para "In some contexts:")
 (para #:align 'center (bit "datatype-oriented") "vs." (bit "object-oriented"))
 (para "... choice of organization with implications for extensibility")
 'next
 (blank)
 (para "In other contexts:")
 (para #:align 'center (bit "functional") "vs." (bit "object-oriented"))
 (para "... choice of language primitives"))

(slide
 #:title "Representing Objects with Higher-Order Functions"
 'alts
 (let ([w1 (code w)]
       [w2 (code w)])
   (define p (scode (define (rectangle #,w1 h)
                      (list
                       (values 'area (lambda () (* #,w2 h)))
                       (values 'bigger-than? (lambda (n) (> (* w h) n)))))))
   (list
    (list p)
    (list
     (pin-arrow-line (/ gap-size 2)
                     p
                     w2 ct-find
                     w1 ct-find
                     #:start-angle (* pi 1/4)
                     #:end-angle (* pi -3/4)
                     #:color "orange"
                     #:line-width 3))))
 (blank)
 (para #:width WIDE "Relies on nested functions")
 (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
   (para #:align 'right "... implemented as closures")))

(slide
 #:title "Representing Objects with First-Order Functions"
 (sscode (define-syntax-rule (rectangle init-w init-h)
           (values (list (values 'w init-w) (values 'h init-h))
                   (list (values 'area (lambda (this)
                                         (* (get this w)
                                            (get this h))))
                         (values 'bigger-than? (lambda (this n)
                                                 (> (send this area)
                                                    n)))))))
 'next
 (para #:width (* 1.1 WIDE) "Could be written as")
 (sscode (define (r-area this)
           (* (get this w) (get this h)))
         code:blank
         (define (r-bigger-than? this n)
           (> (send this area) n))
         code:blank
         (define-syntax-rule (rectangle init-w init-h)
           (values (list (values 'w init-w) (values 'h init-h))
                   (list (values 'area r-area)
                         (values 'bigger-than? r-bigger-than?))))))

(define objs-only-title "Representing Objects with First-Order Functions")

(slide
 #:title objs-only-title
 #:layout 'tall
 (walign
  (sscode
   (code:comment "A Shape is")
   (code:comment " (values (list ....)")
   (code:comment "         (list (values 'area (Shape -> Number))")
   (code:comment "               (values 'bigger-than? (Shape Number -> Number))))")))
 'next
 (walign
  (sscode
   (define-syntax-rule (get o-expr f-id)
     (find (fst o-expr) 'f-id))))
 'next
 (walign
  (sscode
   (define-syntax-rule (rectangle init-w init-h)
     (values (list (values 'w init-w) (values 'h init-h))
             (list (values 'area (lambda (this) 
                                   (* (get this w) (get this h))))
                   (values 'bigger-than? (lambda (this arg)
                                           (> (send this area) arg))))))))
 'next
 (walign
  (sscode
   (define-syntax-rule (send o-expr m-id arg-expr ...)
     (let ([o o-expr])
       ((find (snd o) 'm-id) o arg-expr ...)))
   code:blank
   (define r (rectangle 10 15))
   (send r bigger-than? 200) #,sym:implies #f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 8)

(slide
 #:title objs-only-title
 (para #:width WIDE
       "Simplifiction: assume that all methods take one argument, and the argument is named" (code arg))
 (align
  (sscode
   (define-syntax-rule (object ([field-id field-expr] ...)
                               [method-id (arg) body-expr] ...)
     ....)))
 'next
 (align
  (sscode
   (define-syntax-rule (rectangle init-w init-h)
     (object ([w init-w]
              [h init-h])
             [area (arg) (* (get this w)
                            (get this h))]
             [bigger-than? (arg) (> (send this area 0)
                                    arg)]))
   code:blank
   (define r (rectangle 10 15))
   (send r area 0)#,sym:implies 150
   (send r bigger-than? 100) #,sym:implies #t)))

;; ----------------------------------------
(part 9)

(slide
 #:title "Functions/Datatypes versus Objects"
 (para (it "So far:"))
 (para #:align 'center "object-oriented interpreter of a functional language")
 'next
 (blank)
 (para (it "Now:"))
 (para #:align 'center "functional interpreter of an object-oriented language"))

(define (encode a b)
  (list (scale
         (vl-append
          gap-size
          (hc-append (tt " ") a)
          (tt "≈")
          (hc-append (tt " ") b))
         0.9)))

(define objs-not-funs "Objects Instead of Functions")

(define field-expr (nonterm "Exp"))
(define method-expr (nonterm "Exp"))

(define object-grammar
  (grammar-table
   (list (nonterm "Exp") eqls (t "...") (blank)
         (blank)         -or- (code {object {#,(repeat (nonterm "Field"))} #,(repeat (nonterm "Method"))}) (blank)
         (blank)         -or- (code {get #,(nonterm "Exp") #,(nonterm "Symbol")}) (blank)
         (blank)         -or- (code {send #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Exp")}) (blank)
         (nonterm "Field") eqls (code [#,(nonterm "Symbol") #,field-expr]) (blank)
         (nonterm "Method") eqls (code [#,(nonterm "Symbol") {arg} #,method-expr]) (blank))))

(define (annote p x dir . l)
  (pin-balloon (wrap-balloon (apply para #:fill? #f #:width (* client-w 1/3) l)
                             dir
                             (case dir
                               [(w) (- gap-size)]
                               [else 0])
                             (case dir
                               [(w) 0]
                               [(nw) (- gap-size)]))
               p
               x cc-find))

(slide
 #:title objs-not-funs
 'alts
 (list
  (list
   object-grammar)
  (list
   (annote object-grammar field-expr 'nw "evaluated when object is created"))
  (list
   (annote object-grammar method-expr 'nw "delayed until method is called"))
  (list
   (annote object-grammar method-expr 'nw "method always takes one argument"))
  (list
   (annote object-grammar method-expr 'nw "use" (code arg) "to access method argument"))
  (list
   (annote object-grammar method-expr 'nw "use" (code this) "to access enclosing object"))
  (list
   (annote object-grammar field-expr 'nw (code arg) "and" (code this) "refer to outside object, if any"))))

(slide
 #:title objs-not-funs
 (scale object-grammar 0.9)
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (code {object
          {[x 1] [y 2]}}))
  (list
   (code {object
          {[x 1] [y {+ 1 1}]}}))
  (list
   (code {get {object
               {[x 1] [y 2]}}
              x}
         #,sym:implies
         1))
  (list
   (code {object
          {}
          [inc {arg} {+ arg 1}]}))
  (list
   (code {send {object
                {}
                [inc {arg} {+ arg 1}]}
               inc
               2}
         #,sym:implies
         3))
  (list
   (code {object
          {}
          [inc {arg} {+ arg 1}]
          [dec {arg} {+ arg -1}]}))
  (list
   (code {object
          {[x 1] [y 2]}
          [mdist {arg} {+ {get this x}
                          {get this y}}]}))
  (list
   (code {send {object
                {[x 1] [y 2]}
                [mdist {arg} {+ {get this x}
                                {get this y}}]}
               mdist
               0}
         #,sym:implies
         3))))
   
;; ----------------------------------------
(part 10)

(slide
 #:title "Expressive Power"
 (para "Functions can encode objects")
 (scode (list (values 'area (lambda () (* w h)))
              (values 'bigger-than? (lambda (n) (> (* w h) n)))))
 'next
 (blank gap-size)
 (para "Objects can encode functions?"))

(slide
 #:title objs-not-funs
 (para #:width (* 0.9 (get-client-w #:aspect 'fullscreen)) "Objects can encode functions:")
 'next
 (blank)
 'alts
 (list
  (encode
   (code {{lambda {x} x}
          5})
   (code {send {object {} [call {arg} arg]} 
               call
               5}))
  (encode
   (code {{{lambda {x} {lambda {y} {+ x y}}}
           5}
          6})
   (code
    {send {send {object 
                 {}
                 [call {arg} {object
                              {[x arg]}
                              [call {arg} {+ arg 
                                             {get this x}}]}]}
                call
                5}
          call
          6}))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 11)

(define (ajava  . l)
  (inset
   (let ([p (frame
             (inset
              (scale/improve-new-text 
               (vl-append
                (apply java l))
               0.6)
              (* 2 (current-line-sep))))])
     (refocus (vc-append
               (current-line-sep)
               (scale (t "Analogous Java code") 0.5)
               p)
              p))
   0 (- gap-size) (- (* gap-size 2)) 0))

(define (analogous-to n j)
  (ht-append gap-size n j))

(slide
 #:title "Object Language"
 (scale/improve-new-text
  (grammar-table
   (list (nonterm "Exp") eqls (nonterm "Number") (blank)
         (blank)         -or- (code {+ #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)         -or- (code {* #,(nonterm "Exp") #,(nonterm "Exp")}) (blank)
         (blank)         -or- (code arg) (blank)
         (blank)         -or- (code this) (blank)
         (blank)         -or- (code {object {#,(repeat (nonterm "Field"))} #,(repeat (nonterm "Method"))}) (blank)
         (blank)         -or- (code {get #,(nonterm "Exp") #,(nonterm "Symbol")}) (blank)
         (blank)         -or- (code {send #,(nonterm "Exp") #,(nonterm "Symbol") #,(nonterm "Exp")}) (blank)))
  0.8)
 'next
 (blank)
 'alts
 (list (list
        (blank)
        (blank)
        (analogous-to
         (scale
          (sshcode {send {object
                          {[x 3] [y 4]}
                          [mdist {arg} {+ {get this x} 
                                          {get this y}}]}
                         call
                         mdist
                         0})
          1.2)
         (ajava "class Posn {"
                "  int x, y;"
                "  int mdist() {"
                "    return this.x + this.y;"
                "  }"  
                "}"
                "new Posn(3,4).mdist()")))
       (list
        (analogous-to
         (sshcode {send {object
                         {[x 1] [y 2] [z 3]}
                         [mdist {arg} {+ {get this x} 
                                         {+ {get this y}
                                            {get this z}}}]
                         [addDist {arg} {+ {send arg mdist 0}
                                           {send this mdist 0}}]}
                        addDist
                        {object
                         {[x 3] [y 4]}
                         [mdist {arg} {+ {get this x} 
                                         {get this y}}]}})
         (ajava "class Posn {"
                "  ... as before ..."
                "}"
                "class Posn3D extends Posn {"
                "  int z; ..."
                "  int mdist() {"
                "    return this.x + this.y + this.z;"
                "  }"
                "  int addDist(Posn p) {"
                "    return p.mdist() + this.mdist();"
                "  }"
                "}"
                "new Posn3D(1,2,3).addDist(new Posn(3,4))")))))

;; ----------------------------------------

(slide
 #:title "Expressions"
 (sscode
  (define-type Exp
    (numE [n : Number])
    (plusE [lhs : Exp]
           [rhs : Exp])
    (multE [lhs : Exp]
           [rhs : Exp])
    (argE)
    (thisE)
    (objectE [fields : (Listof (Symbol * Exp))]
             [methods : (Listof (Symbol * Exp))])
    (getE [obj-expr : Exp]
          [field-name : Symbol])
    (sendE [obj-expr : Exp]
           [method-name : Symbol]
           [arg-expr : Exp]))))

(slide
 #:title "Values"
 (scode
  (define-type Value
    (numV [n : Number])
    (objV [fields : (Listof (Symbol * Value))]
          [methods : (Listof (Symbol * Exp))]))))

(slide
 #:title "Examples"
 'alts
 (list
  (list
   (scode
    interp : (Exp -> Value)
    code:blank
    (test (interp (plusE (numE 1) (numE 2)))
          (numV 3))))
  (list
   (scode
    (test (interp (objectE empty empty))
          (objV empty empty))))
  (list
   (scode
    (test (interp (objectE (list
                            (values 'x (plusE (numE 1)
                                              (numE 2))))
                           (list 
                            (values 'inc (plusE (argE) 
                                                (numE 1))))))
          (objV (list (values 'x (numV 3)))
                (list (values 'inc (plusE (argE) 
                                          (numE 1))))))))
  (list
   (scode
    (test (interp (getE
                   (objectE (list
                             (values 'x (plusE (numE 1) 
                                               (numE 2))))
                            (list 
                             (values 'inc (plusE (argE) 
                                                 (numE 1)))))
                   'x))
          (numV 3))))
  (list
   (scode
    (test (interp (sendE 
                   (objectE (list
                             (values 'x (plusE (numE 1) 
                                               (numE 2))))
                            (list
                             (values 'inc (plusE (argE) 
                                                 (numE 1)))))
                   'inc
                   (numE 7)))
          (numV 8))))
  (list
   (scode
    (test (interp (plusE (argE) (numE 1)))
          ???))
   'next
   (blank)
   (para "Need" (code arg) "and" (code this) "values...")
   'next
   (para "Instead of an environment, just provide 2 values to" (code interp)))
  (list
   (scode
    (define interp : (Exp Value Value -> Value)
      (lambda (a this-val arg-val)
        ...))
    code:blank
    (test (interp (plusE (argE) (numE 1))
                  (objV empty empty)
                  (numV 7))
          (numV 8))))
  (list
   (scode
    (test (interp (getE (thisE) 'x)
                  (objV (list (values 'x (numV 9)))
                        empty)
                  (numV 7))
          (numV 9))))
  (list
   (scode
    (test (interp (plusE (numE 1) (numE 2))
                  (objV empty empty)
                  (numV 0))
          (numV 3))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 12)

(define (interp-slide keep? middle)
  (if (or keep? (not condense?))
      (slide
       #:title "Interpreter"
       #:layout 'top
       (para
        #:width (get-client-w #:aspect 'fullscreen)
        (sscode
         (define interp : (Exp Value Value -> Value)
           (lambda (a this-val arg-val)
             (type-case Exp a
               ...
               #,(un-ss-scale middle)
               ...))))))
      (skip-slides 1)))

(interp-slide
 #f
 (blank))

(interp-slide
 #t
 (sscode
  [(numE n) (numV n)]
  [(plusE l r) (num+ (interp l this-val arg-val) 
                     (interp r this-val arg-val))]
  [(multE l r) (num* (interp l this-val arg-val) 
                     (interp r this-val arg-val))]
  [(thisE) this-val]
  [(argE) arg-val]))

;; ---

(interp-slide
 #f
 (sscode
  [(objectE fields methods)
   ....]))

(interp-slide
 #f
 (sscode
  [(objectE fields methods)
   .... (map (lambda (f)
               .... f ....)
             fields)
   ....]))

(interp-slide
 #f
 (sscode
  [(objectE fields methods)
   .... (map (lambda (f)
               (let ([name (fst f)]
                     [exp (snd f)])
                 ....))
             fields)
   ....]))

(interp-slide
 #t
 (sscode
  [(objectE fields methods)
   .... (map (lambda (f)
               (let ([name (fst f)]
                     [exp (snd f)])
                 .... (interp exp this-val arg-val) ....))
             fields)
   ....]))

(interp-slide
 #t
 (sscode
  [(objectE fields methods)
   (objV (map (lambda (f)
                (let ([name (fst f)]
                      [exp (snd f)])
                  .... (interp exp this-val arg-val) ....))
              fields)
         methods)]))

(interp-slide
 #t
 (sscode
  [(objectE fields methods)
   (objV (map (lambda (f)
                (let ([name (fst f)]
                      [exp (snd f)])
                  (values name
                          (interp exp this-val arg-val))))
              fields)
         methods)]))

;; ---

(interp-slide
 #f
 (sscode
  [(getE obj-expr field-name)
   ....]))

(interp-slide
 #f
 (sscode
  [(getE obj-expr field-name)
   .... (interp obj-expr this-val arg-val) ....]))

(interp-slide
 #f
 (sscode
  [(getE obj-expr field-name)
   (type-case Value (interp obj-expr this-val arg-val)
     [(objV fields methods)
      ....]
     [else (error 'interp "not an object")])]))

(interp-slide
 #t
 (sscode
  [(getE obj-expr field-name)
   (type-case Value (interp obj-expr this-val arg-val)
     [(objV fields methods)
      (find fields field-name)]
     [else (error 'interp "not an object")])]))

;; ---

(interp-slide
 #f
 (sscode
  [(sendE obj-expr method-name arg-expr)
   ....]))

(interp-slide
 #f
 (sscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj
             (interp obj-expr this-val arg-val))
           (define arg-val
             (interp arg-expr this-val arg-val))]
     ....)]))

(interp-slide
 #f
 (sscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj
             (interp obj-expr this-val arg-val))
           (define arg-val
             (interp arg-expr this-val arg-val))]
     (type-case Value obj
       [(objV fields methods)
        ....]
       [else (error 'interp "not an object")]))]))

(interp-slide
 #f
 (sscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj
             (interp obj-expr this-val arg-val))
           (define arg-val
             (interp arg-expr this-val arg-val))]
     (type-case Value obj
       [(objV fields methods)
        .... (find methods method-name)
        ....]
       [else (error 'interp "not an object")]))]))

(interp-slide
 #f
 (sscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj
             (interp obj-expr this-val arg-val))
           (define arg-val
             (interp arg-expr this-val arg-val))]
     (type-case Value obj
       [(objV fields methods)
        (let ([body-expr (find methods method-name)])
          ....)]
       [else (error 'interp "not an object")]))]))

(interp-slide
 #t
 (sscode
  [(sendE obj-expr method-name arg-expr)
   (local [(define obj
             (interp obj-expr this-val arg-val))
           (define arg-val
             (interp arg-expr this-val arg-val))]
     (type-case Value obj
       [(objV fields methods)
        (let ([body-expr (find methods method-name)])
          (interp body-expr
                  obj
                  arg-val))]
       [else (error 'interp "not an object")]))]))
