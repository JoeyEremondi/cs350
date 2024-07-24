#lang at-exp slideshow
(require slideshow/code
         slideshow/repl
         "utils/utils.rkt"
         "utils/in-file.rkt")

(current-keyword-list
 (cons "syntax-parse" (current-keyword-list)))

(define repl (make-repl-group))

(define larger-font-size 32)
(define default-font-size 28)
(define smaller-font-size 24)

(define (sole-repl-area #:height [h (* (+ 5 default-font-size) 4)]
                        #:font-size [font-size default-font-size]
                        . content)
  (apply repl-area
         #:font-size font-size
         #:height h
         content))

(define (normal-result-area)
  (result-area repl
               #:width (* client-w 2/3)
               #:height (* client-h 1/4)))

(define small-w (* client-w 1/3))
(define small-h (* client-h 1/3))

(define med-w (* client-w 1/2))
(define med-h (* client-h 1/2))

(define big-w (* client-w 2/3))
(define big-h (* client-h 1/2))

(define (two-modules a-mod b-mod
                     #:font-size [font-size #f]
                     #:width [width small-w]
                     #:left-width [left-width width]
                     #:right-width [right-width width]
                     #:height [height small-h]
                     #:left-height [left-height height]
                     #:right-height [right-height height])
  (hc-append
   gap-size
   (normal-module-area a-mod 
                       #:font-size font-size
                       #:width left-width 
                       #:height left-height)
   (normal-module-area b-mod
                       #:font-size font-size
                       #:width right-width
                       #:height right-height)))

(define (normal-module-area mod
                            #:font-size [font-size #f]
                            #:width w
                            #:height h)
  (in-file
   (scale (tt (module-backing-module-name mod)) 0.85)
   (module-area mod
                #:width w
                #:height h
                #:font-size (or font-size default-font-size)
                #:background file-background)))

;; ----------------------------------------
(part 1)

(slide
 #:title "DrRacket is an Interpreter"
 (bitmap "drracket.png"))

(define (instead p) (colorize p "blue"))

(define (result s) (colorize (tt s) "darkblue"))

(slide
 #:title (code eval)
 (para "The" (code eval) "function is a built-in"
       (code parse) "plus" (code interp) ":")
 (blank)
 (cond
  [printing?
   (code
    > (eval '(+ 1 2))
    #,(result "3")
    > (eval '((lambda (x) (+ x x))
              3))
    #,(result "6")
    > (define op '+)
    > (eval `(,op 1 2))
    #,(result "3")
    > (define name 'x)
    > (eval `(define ,name 3))
    > x
    #,(result "3"))]
  [else
   (sole-repl-area #:height med-h
                   #:font-size larger-font-size)]))
 
(define no-eval-title (hbl-append (titlet "Misuse of ") (code eval)))

(slide
 #:title no-eval-title
 (para "A bad use of" (code eval) ":")
 (scode (define (three-times f-name arg-expr)
          (eval `(,f-name
                  (,f-name
                   (,f-name ,arg-expr)))))
        code:blank
        (three-times 'sqrt '256)
        (code:contract = (eval '(sqrt (sqrt (sqrt 256))))))
 'next
 (blank)
 (instead (para "Instead:"))
 (scode (define (three-times f arg)
          (f (f (f arg))))
        code:blank
        (three-times sqrt 256)))

(slide
 #:title no-eval-title
 (para #:width (* client-w 0.9) "Another bad use, though less trivial:")
 'alts
 (let ([p (code
           (struct posn (x y))
           code:blank
           (posn-x (posn 1 2))
           (posn-y (posn 1 2))
           code:blank
           (define (get p field)
             (define getter
               (eval (string->symbol
                      (string-append
                       "posn-" 
                       (symbol->string field)))))
             (getter p))
           code:blank
           (get (posn 1 2) 'x))])
   (list
    (list p)
    (list (rb-superimpose p
                          (instead (t "Alternative: a macro")))))))


(slide
 #:title no-eval-title
 (para "A use of" (code eval) " that doesn't work:")
 (scode (define (make-table expr)
          (map (lambda (x)
                 (map (lambda (y) (eval expr))
                      (list 1 2 3)))
               (list 1 2 3)))
        code:blank
        (make-table '(- x y)))
 'next
 (blank)
 (para "If you think of" (code eval) "as" (code parse) 
       "plus" (code interp) ", the call to" (code interp)
       "starts with an empty environment")
 'next
 (para #:align 'right "... sort of"))

(slide
 #:title no-eval-title
 (para "Yet another a bad use of" (code eval) ":")
 (sscode (define (make-table expr)
           (map (lambda (x)
                  (map (lambda (y) (eval `(let ([x ,x]
                                                [y ,y])
                                            ,expr)))
                       (list 1 2 3)))
                (list 1 2 3)))
         code:blank
         (make-table '(- x y)))
 'next
 (instead (para "Instead:"))
 (sscode (define (make-table f)
           (map (lambda (x)
                  (map (lambda (y)
                         (f x y))
                       (list 1 2 3)))
                (list 1 2 3)))
         code:blank
         (make-table (lambda (x y) (- x y)))))

(slide
 #:title no-eval-title
 (para "Fails:")
 (scode
  (define (three-times f-name arg-expr)
    (eval `(,f-name
            (,f-name
             (,f-name ,arg-expr)))))
  code:blank
  (let ([double (lambda (x) (+ x x))])
    (three-times 'double '2)))
 'next
 (blank)
 (instead (para "Instead:"))
 (scode
  (define (three-times f arg)
    (f (f (f arg))))
  code:blank
  (let ([double (lambda (x) (+ x x))])
    (three-times double 2))))

(slide
 (para #:align 'center "First, do no" (code eval)))

;; ----------------------------------------
(part 2)

(define namespaces-title "Namespaces")

(slide
 #:title namespaces-title
 (para (code eval) "starts with the current" (bit "namespace"))
 'next
 (item "Interactions area: module's top-level definitions")
 (inset (inset (bitmap "repl.png") (- gap-size)) 0 0 0 (- gap-size))
 'next
 (item "During module body: empty")
 (code #,(tt "#lang racket")
       (eval '(+ 1 2)) (code:comment "fails")))

(slide
 #:title namespaces-title
 (para "Using an explicit namespace:")
 (blank)
 (code
  #,(tt "#lang racket")
  code:blank
  (define ns (make-base-namespace))
  (eval '(+ 1 2) ns) (code:comment "⇒ 3")
  code:blank
  (eval '(define x 10) ns)
  (eval 'x ns) (code:comment "⇒ 10")
  code:blank
  (define y 11)
  (eval 'y ns) (code:comment "⇒ error")))

(slide
 #:title no-eval-title
 (para "A good use of" (code eval) ":")
 (blank)
 (scode (define (check-handin e)
          ....
          (define ns (make-base-namespace))
          (eval e ns)
          ....)))

(slide
 #:title (hbl-append (titlet "The Problem of ") (code eval))
 (item "First, do no" (code eval))
 'next
 (blank)
 (item "A necessary" (code eval) "⇒ use an explicit namespace")
 'next
 (blank)
 (blank)
 (blank)
 (colorize
  (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
    (para #:align 'right "See also Richards et al., ``The Eval that Men Do''"))
  "blue"))


;; ----------------------------------------
(part 3)

(define expressive-title "Expressive Macros")

(slide
 #:title expressive-title
 (code
  (struct posn (x y))
  code:blank
  (posn-x (posn 1 2))
  (posn-y (posn 1 2))
  code:blank
  (define-syntax-rule (get p field)
    (code:comment "Doesn't work:")
    ((string->symbol
      (string-append
       "posn-" 
       (symbol->string 'field)))
     p))
  code:blank
  (get (posn 1 2) x)))

(slide
 #:title expressive-title
 (para "The" (code define-syntax) "form binds a compile-time function as a macro:")
 (code
  (define-syntax get
    (lambda (stx)
      ....)))
 'next
 'alts
 (list
  (list
   (blank)
   (para "The" (code syntax-rules) "and" (code syntax-id-rules) "forms actually produce functions"))
  (list
   (para "With")
   (code (get (posn 1 2) x))
   (para "the" (code stx) "argument is something like")
   (code '(get (posn 1 2) x))
   (para "and the result is something like")
   (code '(posn-x (posn 1 2))))))

(define x-mod
  (make-module-backing
   repl
   #:module-name "x.rkt"
   "#lang racket"
   ""
   "(define x 1)"
   ""
   "(define-syntax-rule (get)"
   "  x)"
   ""
   "(provide get)"))

(define y-mod
  (make-module-backing
   repl
   #:module-name "y.rkt"
   "#lang racket"
   ""
   "(require \"x.rkt\")"
   ""
   "(define x 2)"
   ""
   "(get)"))

(slide
 #:title "Macro Results"
 (two-modules x-mod y-mod
              #:left-width med-w
              #:left-height med-h)
 'alts
 (list
  (list
   (normal-result-area))
  (list
   (para #:fill? #f (code 'code:blank) "produces an" (bit "s-expression"))
   (para #:fill? #f (code #'code:blank) "produces a" (bit "syntax object")))))
;; Change `get` to `(define-syntax get (lambda (stx) 'x))` [fails]
;; Change `get` to `(define-syntax get (lambda (stx) #'x))`
;; Change `get` to `(define-syntax (get stx) #'x)`

;; ----------------------------------------
(part 4)

(define syntax-objects-title "Syntax Objects")

(slide
 #:title syntax-objects-title
 (item "Variable reference:")
 (sole-repl-area "x")
 (item "Symbol s-expression:")
 (sole-repl-area "'x")
 (item "Syntax object:")
 (sole-repl-area "#'x"))

(slide
 #:title syntax-objects-title
 (item "Function call:")
 (sole-repl-area "(+ 1 2)")
 (item "List s-expression:")
 (sole-repl-area "'(+ 1 2)")
 (item "Syntax object:")
 (sole-repl-area "#'(+ 1 2)"))

(slide
 #:title syntax-objects-title
 (sole-repl-area #:height big-h))
;; #'x
;; (syntax-e #'x)
;; #'2
;; (syntax-e #'2)
;; #'(+ 1 2)
;; (syntax-e #'(+ 1 2))
;; (datum->syntax #'here 1)

;; ----------------------------------------
(part 5)

(define tick-mod
  (make-module-backing
   repl
   #:module-name "tick.rkt"
   "#lang racket"
   ""
   "(define sound \"tick\")"
   ""
   "(define id 'sound)"
   "(provide id)"))

(define tock-mod
  (make-module-backing
   repl
   #:module-name "tock.rkt"
   "#lang racket"
   ""
   "(define sound \"tock\")"
   ""
   "(require \"tick.rkt\")"))

(slide
 #:title "Symbols and Syntax Objects"
 (two-modules tick-mod tock-mod
              #:font-size smaller-font-size)
 (normal-result-area))
;; Symbols have no scope...
;;  (eq? id sound)
;;  (eval 'sound)
;;  (eval #'sound)
;;  (eval id)
;; Note that ' is an abbreviation for `quote' [in `tick']
;; Show that #' is an abbreviation for `quote-syntax' [in `tick']
;;  #`(list #,id "!")
;;  (eval #`(list #,id "!")) [in `tock']

;; ----------------------------------------
(part 6)

(define now-mod
  (make-module-backing
   repl
   #:module-name "now.rkt"
   "#lang racket"
   ""
   "(define now"
   "  (current-seconds))"
   ""
   "now"))

(slide
 #:title "Macros"
 (normal-module-area now-mod 
                     #:width med-w
                     #:height med-h)
 (normal-result-area))
;; Try `now (sleep 1) now'
;; Change `now' to a macro
;;  Aside:
;;   `(now 1)` is allowed...
;;   (if (symbol? (syntax-e stx))
;;       #'(current-seconds)
;;       (raise-syntax-error 'now "bad syntax"))
;;  Change it back for simplicity

(define then-mod
  (make-module-backing
   repl
   #:module-name "then.rkt"
   "#lang racket"
   ""
   "(define-syntax (then stx)"
   "  (current-seconds))"
   ""
   "then"))

(slide
 #:title "Compile-Time Expressions"
 (normal-module-area then-mod
                     #:width med-w
                     #:height med-h)
 (normal-result-area))
;; Fix bug by using `datum->syntax` #`#,
;; `then', `(sleep 1)' , `then'
;; Define `around-then' using quasisyntax

;; ----------------------------------------
(part 7)

(slide
 #:title "Compile-Time Imports"
 (normal-module-area then-mod
                     #:width big-w
                     #:height med-h)
 (normal-result-area))
;; Change `racket' to `racket/base'
;;  ... add `(require (for-syntax racket/base))'

(define recent-mod
  (make-module-backing
   repl
   #:module-name "recent.rkt"
   "#lang racket/base"
   ""
   "(define (recent-seconds)"
   "  (- (current-seconds) 10))"
   ""
   "(provide recent-seconds)"))

(define about-then-mod
  (make-module-backing
   repl
   #:module-name "about-then.rkt"
   "#lang racket/base"
   "(require (for-syntax racket/base"
   "                     \"recent.rkt\"))"
   ""
   "(define-syntax (about-then stx)"
   "  #`#,(recent-seconds))"
   ""
   "about-then"))

(slide
 #:title "Compile-Time Imports"
 (vc-append gap-size
            (normal-module-area recent-mod
                                #:font-size smaller-font-size
                                #:width big-w 
                                #:height (* client-h 1/4))
            (normal-module-area about-then-mod
                                #:font-size smaller-font-size
                                #:width big-w
                                #:height small-h))
 (normal-result-area))
;; Mangle import of "recent.rkt"

;; ----------------------------------------
(part 8)

(define now-alias-mod
  (make-module-backing
   repl
   #:module-name "main.rkt"
   @string-append{
                    #lang racket
                    (require (for-syntax racket/list))
                          
                    (define-syntax (define-now-alias stx)
                      (define id (second (syntax-e stx)))
                      #`(define-syntax (#,id stx)
                          #'(current-seconds)))
                      
                    (define-now-alias now)
                    (define-now-alias at-this-moment)
                          
                    at-this-moment (sleep 1) now
                   }))

(slide
 #:title "Macro-Generating Macros"
 (normal-module-area now-alias-mod 
                     #:width big-w
                     #:height big-h)
 (normal-result-area))

(slide
 #:title "Macro-Generating Macros and Imports"
 (vc-append
  (/ gap-size 2)
  (normal-module-area recent-mod 
                      #:font-size smaller-font-size
                      #:width big-w
                      #:height (* client-h 1/6))
  (normal-module-area now-alias-mod 
                      #:font-size smaller-font-size 
                      #:width big-w
                      #:height (- big-h (* 2 gap-size))))
 (normal-result-area))
;; Convert alias creator to `recently'
;; Convert alias creator to `about-then'

;; ----------------------------------------
(part 9)

(define-code CODE typeset-code UNSYNTAX)

(slide
 #:title "Pattern Matching vs. Arbitrary Expressions"
 (code
  (define-five x)
  x (code:comment "⇒ 5"))
 (blank)
 'next
 (para "Easy:")
 (scale
  (code
   (define-syntax-rule (define-five name)
     (define name 5)))
  0.85)
 'next
 (blank)
 (para "Painful and incomplete:")
 (scale
  (CODE
   (define-syntax define-five
     (lambda (stx)
       (define id (second (syntax-e stx)))
       #`(define #,id 5))))
  0.85))

(slide
 #:title "Pattern Matching plus Arbitrary Expressions"
 (scale
  (CODE
   (require (for-syntax syntax/parse))
   code:blank
   (define-syntax define-five
     (lambda (stx)
       (syntax-parse stx
         [(define-five name)
          (unless (symbol? (syntax-e #'name))
            (raise-syntax-error 'define-now-alias
                                "not an identifier"))
          #'(define name 5)]))))
  0.85)
 'next
 (blank)
 (blank)
 (item (code syntax-parse) "patterns bind pattern variables")
 (item (code #'code:blank) "templates can use pattern variables"))

(slide
 #:title (hbl-append (code syntax-rules) (titlet " and ") (code syntax-parse))
 (scode (define-syntax rotate
          (syntax-rules ()
            [(rotate a) (void)]
            [(rotate a b c ...) (begin
                                  (swap a b)
                                  (rotate b c ...))])))
 (para #:width client-w "is the same as")
 (scode (define-syntax (rotate stx)
          (syntax-parse stx
            [(rotate a) #'(void)]
            [(rotate a b c ...) #'(begin
                                    (swap a b)
                                    (rotate b c ...))]))))

(slide
 #:title "Error Checking"
 (scode (define-syntax (rotate stx)
          (syntax-parse stx
            [(rotate a)
             (unless (symbol? (syntax-e #'a))
               (raise-syntax-error 'rotate
                                   "not an identifier"))
             #'(void)]
            [(rotate a b c ...)
             (unless (symbol? (syntax-e #'a))
               (raise-syntax-error 'rotate
                                   "not an identifier"))
             #'(begin
                 (swap a b)
                 (rotate b c ...))]))))

(slide
 #:title "Simpler Error Checking"
 (scode (define-syntax (rotate stx)
          (syntax-parse stx
            [(rotate a:id)
             #'(void)]
            [(rotate a:id b:id c:id ...)
             #'(begin
                 (swap a b)
                 (rotate b c ...))])))
 'next
 (blank)
 (blank)
 (item (code id) "is a pre-defined" (bit "syntax class"))
 'next
 (item (hbl-append (code _sym) (code :) (code _class)) "constrains" (code _sym) "to match" (code _class)))
