#lang at-exp slideshow
(require slideshow/code
         slideshow/repl
         "utils/utils.rkt"
         "utils/in-file.rkt")

(current-keyword-list
 (list* "syntax-parse"
        "define-syntax-parameter"
        "syntax-parameterize"
        (current-keyword-list)))

(define-code CODE typeset-code UNSYNTAX)

(define method-macro-title "Method Macro")

(define expands (colorize (t "⇒") "blue"))
(define expands+ (inset expands 0 (/ gap-size 2)))

;; ----------------------------------------
(part 1)

(define ex-body (code (+ arg (get this x))))

(slide
 #:title method-macro-title
 (para "Suppose that we want")
 (code (method #,ex-body))
 (para "to expand to")
 (code (lambda (this arg)
         #,ex-body)))

(define ex-expansion
  (code (lambda^1 (this^1 arg^1)
          #,ex-body)))

(slide
 #:title method-macro-title
 (code (define-syntax-rule (method expr)
         (lambda (this arg)
           expr)))
 'next
 (blank)
 (para "Doesn't work:")
 (blank)
 (code (method #,ex-body)
       #,expands+
       #,ex-expansion))

(slide
 #:title "Lexical Context"
 ex-expansion
 (para "The superscripts correspond to"
       (bit "lexical context"))
 'next
 (blank)
 (para "We can use" (code datum->syntax) "to manipulate lexical context"))

(slide
 #:title "Non-Hygienic Macros"
 (scale
  (CODE (define-syntax (method stx)
          (syntax-parse stx
            [(method expr)
             (define this-var (datum->syntax #'expr 'this))
             (define arg-var (datum->syntax #'expr 'arg))
             #`(lambda (#,this-var #,arg-var)
                 expr)])))
  0.9)
 (blank)
 (blank)
 'alts
 (list
  (list
   (para "Using" (code (datum->syntax #'expr 'this)) "gives" (code this)
         "the lexical context of" (code #'expr)))
  (list
   (code (method #,ex-body)
         #,expands+
         (lambda^1 (this arg)
           #,ex-body)))))

;; ----------------------------------------
(part 2)

(define (align p) (para #:width (* 0.9 client-w) p))

(slide
 #:title "Non-Hygienic Macros and Composition"
 (align
  (CODE (define-syntax-rule (method-plus-one expr)
          (method (+ 1 expr)))))
 (blank)
 (blank)
 'next
 (align
  (code (method-plus-one arg)))
 'next
 (align expands)
 (align (code (method (+^2 1^2 arg))))
 'next
 (align expands)
 (align
  (code
   (lambda^3 (this^2 arg^2)
     (+^2 1^2 arg)))))

;; ----------------------------------------
(part 3)

(slide
 #:title "Alternative to Non-Hygienic Macros"
 (para "Bind" (code this) "and" (code arg) "as macros in the same scope as" (code method) ":")
 (code (define-syntax method ....)
       (define-syntax this ....)
       (define-syntax arg ....))
 'next
 (blank)
 (para "Make the" (code this) "and" (code arg) "macros cooperate with the" (code method) "macro"))

(define stxparam-title "Syntax Parameters")

(slide
 #:title stxparam-title
 (para "The" (code racket/stxparam) "library provides")
 (item (code define-syntax-parameter) "--- to introduce a cooperation channel")
 (item (code syntax-parameterize) "--- to send through the channel")
 (item (code syntax-parameter-value) "--- to receive from the channel"))


(slide
 #:title stxparam-title
 (scale
  (code
   (define-syntax-parameter this-name #f)
   (define-syntax-parameter arg-name #f)
   code:blank
   (define-syntax this
     (lambda (stx) 
       (define v (syntax-parameter-value #'this-name))
       (if v
           v
           (raise-syntax-error 'this 
                               "illegal outside of method"
                               stx))))
   (define-syntax arg ....)
   code:blank
   (define-syntax-rule (method expr)
     (lambda (this-n arg-n)
       (syntax-parameterize ([this-name #'this-n]
                             [arg-name #'arg-n])
         expr))))
  0.85))

;; ----------------------------------------
(part 4)

(define class-macro-title "Class Macro")

(define (class-macro tmpl)
  (code
   (define-syntax (class stx)
     (syntax-parse stx
       [{class name:id
          {fld:id ...}
          {mthd-name:id mthd-expr} ...}
        #,tmpl]))))

(slide
 #:title class-macro-title
 (para "Let's implement" (code class) "as a macro:")
 (code
  {class posn
    {x y}
    {mdist {+ {get this y}
              {get this x}}}
    {addDist {+ {send this mdist 0}
                {send arg mdist 0}}}})
 'next
 (blank)
 (para "Pattern:")
 (class-macro (code ....)))

(slide
 #:title class-macro-title
 (para #:width (* client-w 0.9) "Template:")
 'alts
 (list
  (list
   (align
    (class-macro
     (code ....))))
  (list
   (align
    (class-macro
     (code #'(....
              (record [fld ???]
                      ...)
              (record [mthd-name (method mthd-expr)]
                      ...))))))
  (list
   (align
    (class-macro
     (code #'(....
              (lambda (fld ...)
                (record [fld fld]
                        ...))
              (record [mthd-name (method mthd-expr)]
                      ...))))))
  (list
   (align
    (code
     #,(class-macro
        (code #'(classV
                 (lambda (fld ...)
                   (record [fld fld]
                           ...))
                 (record [mthd-name (method mthd-expr)]
                         ...))))
     code:blank
     (struct classV (constructor methods)))))
  (list
   (align
    (code
     #,(class-macro
        (code #'(define name
                  (classV
                   (lambda (fld ...)
                     (record [fld fld]
                             ...))
                   (record [mthd-name (method mthd-expr)]
                           ...)))))
     code:blank
     (struct classV (constructor methods)))))))

(define (new-macro tmpl)
  (code
   (define-syntax (new stx)
     (syntax-parse stx
       [{new cls:id expr ...}
        #,tmpl]))))

(slide
 #:title (hbl-append (code new) (titlet " Macro"))
 #:layout 'top
 (code
  {new posn 1 2})
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (align (new-macro (code ....))))
  (list
   (align (new-macro (code .... (classV-constructor cls)
                           ....))))
  (list
   (align (new-macro (code .... ((classV-constructor cls) expr ...)
                           ....))))
  (list
   (align (new-macro (code #'(objectV
                              ....
                              ((classV-constructor cls) expr ...))))))
  (list
   (align (code
           #,(new-macro (code #'(objectV
                                 cls
                                 ((classV-constructor cls) expr ...))))
           code:blank
           (struct objectV (class fields)))))))

;; ----------------------------------------
(part 5)

(define (get-macro tmpl)
  (code
   (define-syntax (get stx)
     (syntax-parse stx
       [{get obj-expr fld:id}
        #,tmpl]))))

(slide
 #:title (hbl-append (code get) (titlet " Macro"))
 #:layout 'top
 (code
  {get this x})
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (align (get-macro (code ....))))
  (list
   (align (get-macro (code .... (objectV-fields obj-expr) ....))))
  (list
   (align (get-macro (code #'(hash-ref (objectV-fields obj-expr)
                                       'fld)))))))
          
(define (send-macro tmpl)
  (code
   (define-syntax (send stx)
     (syntax-parse stx
       [{send obj-expr mthd:id arg-expr}
        #,tmpl]))))

(slide
 #:title (hbl-append (code send) (titlet " Macro"))
 #:layout 'top
 (code
  {send this mdist 0})
 'next
 (blank)
 (blank)
 'alts
 (list
  (list (align (send-macro (code ....))))
  (list (align (send-macro (code #'(let ([obj obj-expr])
                                     ....)))))
  (list (align (send-macro (code #'(let ([obj obj-expr])
                                     .... (objectV-class obj)
                                     ....)))))
  (list (align (send-macro (code #'(let ([obj obj-expr])
                                     .... (classV-methods
                                           (objectV-class obj))
                                     ....)))))
  (list (align (send-macro (code #'(let ([obj obj-expr])
                                     ....
                                     (hash-ref (classV-methods
                                                (objectV-class obj))
                                               'mthd)
                                     ....)))))
  (list
   (align (send-macro (code
                       #'(let ([obj obj-expr])
                           ((hash-ref (classV-methods
                                       (objectV-class obj))
                                      'mthd)
                            obj
                            arg-expr))))))))

(define (ssend-macro tmpl)
  (code
   (define-syntax (ssend stx)
     (syntax-parse stx
       [{ssend obj-expr cls:id mthd:id arg-expr}
        #,tmpl]))))

(slide
 #:title (hbl-append (code ssend) (titlet " Macro"))
 #:layout 'top
 (code
  {ssend this posn mdist 0})
 'next
 (blank)
 (blank)
 'alts
 (list
  (list (align (ssend-macro (code ....))))
  (list (align (ssend-macro (code #'(let ([obj obj-expr])
                                      ....)))))
  (list (align (ssend-macro (code #'(let ([obj obj-expr])
                                      .... (classV-methods cls)
                                      ....)))))
  (list (align (ssend-macro (code #'(let ([obj obj-expr])
                                      ....
                                      (hash-ref (classV-methods cls)
                                                'mthd)
                                      ....)))))
  (list
   (align (ssend-macro (code
                        #'(let ([obj obj-expr])
                            ((hash-ref (classV-methods cls)
                                       'mthd)
                             obj
                             arg-expr))))))))

;; ----------------------------------------
(part 6)

(define class-lang-title "Class Language")

(slide
 #:title class-lang-title
 (in-file
  "posn.rkt"
  (scale
   (code
    #,(tt "#lang s-exp") "class-lang.rkt"
    code:blank
    {class posn
      {x y}
      {mdist {+ {get this y}
                {get this x}}}
      {addDist {+ {send this mdist 0}
                  {send arg mdist 0}}}}
    code:blank
    {class posn3D
      {x y z}
      {mdist {+ {get this z}
                {ssend this posn mdist arg}}}
      {addDist {ssend this posn addDist arg}}}
    code:blank
    {send {new posn3D 5 3 1} addDist {new posn 2 7}})
   0.85)))

(slide
 #:title class-lang-title
 (in-file
  "class-lang.rkt"
  (scale
   (code
    #,(tt "#lang racket")
    (require "method-macro.rkt"
             "class-macro.rkt")
    code:blank
    (provide this arg class new get send ssend)
    code:blank
    ....)
   0.85))
 'next
 (blank)
 (para "Still need:")
 (inset
  (vc-append 
   gap-size
   (item (code +) "and" (code *))
   (item "numbers")
   (item "program is" (hbl-append (code class) (t "es")) "then one expression"))
  gap-size 0 0 0))

(slide
 #:title (hbl-append (code +) (titlet " and ") (code *))
 (code {+ 1 2})
 'next
 (blank)
 (blank)
 'alts
 (list
  (list
   (para "We don't want to provide " (code #%app)))
  (list
   (code
    (provide (rename-out [+-form +]
                         [*-form *]))
    code:blank
    (define-syntax-rule (+-form l r) (+ l r))
    (define-syntax-rule (*-form l r) (* l r))))))


(define numbers-title "Numbers")

(slide
 #:title numbers-title
 (code 1)
 'next
 (blank)
 (blank)
 (para "We don't want to allow strings, booleans, etc."))

(slide
 #:title numbers-title
 (para "A literal is implicitly wrapped by" (code #%datum) ":")
 (code 1
       #,expands+
       (#%datum . 1)  #,expands (quote 1)
       code:blank
       code:blank
       "abc"
       #,expands+
       (#%datum . "abc") #,expands (quote "abc")))

(slide
 #:title numbers-title
 (code
  (provide (rename-out [datum #%datum]))
  code:blank
  (define-syntax (datum stx)
    (syntax-parse stx
      [(datum . v:number)
       #'(#%datum . v)]
      [(datum . other)
       (raise-syntax-error #f
                           "bad syntax"
                           #'other)]))))

;; ----------------------------------------
(part 7)

(define module-body-title "Module Body")

(slide
 #:title module-body-title
 (code
  #,(tt "#lang s-exp") "class-lang.rkt"
  code:blank
  {class _name_1 ....}
  code:blank
  {class _name_2 ....}
  code:blank
  _expr))

(slide
 #:title module-body-title
 (code
  #,(tt "#lang s-exp") "class-lang.rkt"
  ....
  #,expands+
  (module _name "class-lang.rkt"
    (#%module-begin
     ....))))

(define (module-begin-macro tmpl)
  (scale
   (code
    (provide (rename-out [module-begin
                          #%module-begin]))
    code:blank
    (define-syntax (module-begin stx)
      #,tmpl))
   0.85))

(define (module-begin-parse1 tmpl)
  (module-begin-macro
   (code
    (syntax-parse stx
      #:literals (class)
      [(module-begin {class _ ...} ... expr)
       #,tmpl]))))

(define (module-begin-parse2 err tmpl)
  (module-begin-parse1
   (code
    (syntax-parse #'expr
      #:literals (class)
      [{class _ ...}
       #,err]
      [_
       #,tmpl]))))

(define module-begin-err
  (code
   (raise-syntax-error 
    'module-begin
    "need an expression after classes")))

(define module-begin-tmpl
  (code (datum->syntax
         #'here
         (cons #'#%module-begin
               (rest (syntax-e stx))))))


(slide
 #:title module-body-title
 'alts
 (list
  (list (align (module-begin-macro (code ....))))
  (list (align (module-begin-parse1 (code ....))))
  (list (align (module-begin-parse2 (code ....) (code ....))))
  (list (align (module-begin-parse2 module-begin-err (code ....))))
  (list (align (module-begin-parse2 module-begin-err module-begin-tmpl)))))
