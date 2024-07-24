#lang slideshow
(require slideshow/code
         slideshow/balloon
         "utils/macros.rkt"
         "utils/parsing.rkt"
         "utils/utils.rkt"
         "utils/in-file.rkt")

#|
Old part 1:

;; ----------------------------------------
(part 1)

(slide
 #:title "Interpreters"
 (sscode
  {{lambda {mkrec}
     {{lambda {fib}
        {fib 4}}
      {mkrec
       {lambda {fib}
         ;; Fib:
         {lambda {n}
           {if {zero? n}
               1
               {if {zero? {+ n -1}}
                   1
                   {+ {fib {+ n -1}}
                      {fib {+ n -2}}}}}}}}}}
   {lambda {body-proc}
     {{lambda {fX}
        {fX fX}}
      {lambda {fX}
        {body-proc {lambda {x} {{fX fX} x}}}}}}}))

(slide
 #:title "Language Variants"
 (sscode
  {{lambda {mkrec}
     {{lambda {fib}
        {fib 4}}
      {mkrec
       {lambda {fib}
         ;; Fib:
         {lambda {n}
           {if0 n
                1
                {if0 {+ n -1}
                     1
                     {+ {fib {+ n -1}}
                        {fib {+ n -2}}}}}}}}}}
   {lambda {body-proc}
     {{lambda {fX}
        {fX fX}}
      {lambda {fX}
        {body-proc {lambda {x} {{fX fX} x}}}}}}}))

(slide
 #:title "Language Extension"
 (code
  (define-syntax-rule {if0 tst thn els}
    (if (zero? tst)
        thn
        els)))
 'next
 (blank)
 (item (code if0) "is a" (bit "pattern-based macro"))
 'next
 (item "Macros are a form of" (bit "language extension"))
 'next
 (item (bit "Domain-specific languages") "can be extensions")
 'next
 (blank)
 (vc-append
  (current-line-sep)
  (item (bit "Compile-time reflection") "≈" (bit "macros"))
  (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
    (para #:align 'right "... in the Lisp/Scheme/Racket sense")))
 'next
 (blank)
 (colorize
  (para #:align 'center
        #:width (* client-w 0.9)
        "Racket macros can express many languages and extensions")
  "blue"))
|#

;; ----------------------------------------
(part 1)

(define WIDE (* client-w 0.9))

(define case-syntax-rules
  (code
   (syntax-rules (else)
     [(case expr [else rhs])
      (let ([tmp expr])
        rhs)]
     [(case expr [(num) rhs] clause ...)
      (let ([tmp expr])
        (if0 (+ tmp (* -1 num))
             rhs
             (case expr clause ...)))])))

(define case-example
  (sscode
   (let-syntax ([case #,case-syntax-rules])
     (let ([f (lambda (n)
                (case n
                  [(1) 10]
                  [(2) 100]
                  [else 1]))])
       (+ (+ (f 1) (f 2))
          (* -1 (f 3)))))))

(slide
 #:title "Pattern-Based Macros"
 #:layout 'tall
 'alts
 (list
  (list
   (vl-append
    (* 3 (current-line-sep))
    (para #:width WIDE "With" (tt "gensym.rkt") ":")
    (scale
     (code
      (add-let
       `{let-macro {[case {lambda {s}
                           ,(add-let
                             `{let {[tmp {gensym}]}
                               {cons 'let
                                {cons {cons {cons tmp {cons {first {rest s}}
                                                       '{}}}
                                       '{}}
                                 {cons {if0 {if0 {symbol? {first {first {rest {rest s}}}}}
                                             {symbol=? 'else {first {first {rest {rest s}}}}}
                                             1}
                                        {first {rest {first {rest {rest s}}}}}
                                        {cons 'if0
                                         {cons {cons '+
                                                {cons tmp
                                                 {cons {* -1 {first {first {first {rest {rest s}}}}}}
                                                  '{}}}}
                                          {cons
                                           {first {rest {first {rest {rest s}}}}}
                                           {cons {cons 'case
                                                  {cons tmp
                                                   {rest {rest {rest s}}}}}
                                            '{}}}}}}
                                  '{}}}}})}]}
         {let {[f {lambda {n}
                   {case n
                    [{1} 10]
                    [{2} 100]
                    [else 1]}}]}
          {+ {+ {f 1} {f 2}}
             {* -1 {f 3}}}}}))
     0.5)))
  (list
   (blank)
   (para #:width WIDE "In Racket:")
   'alts
   (list
    (list case-example)
    (list (pin-balloon (wrap-balloon (vl-append
                                      (current-line-sep)
                                      (t "Expands to")
                                      (inset
                                       (code 
                                        (lambda (stx)
                                          ....
                                          #,(vl-append
                                             (current-line-sep)
                                             (it "pull expression apart")
                                             (it "and")
                                             (it "rearrange the pieces"))
                                          ....))
                                       gap-size 0 0 0))
                                     'ne 0 (- gap-size))
                       case-example
                       case-syntax-rules (lambda (p q)
                                           (define-values (x y) (lt-find p q))
                                           (define s (tt " "))
                                           (values (+ x (pict-width s))
                                                   (+ y (pict-height s))))))))
  (list
   (blank)
   (para #:width WIDE "In Racket:")
   (blank)
   (sscode
    (let-syntax ([delay (syntax-rules ()
                         [(delay expr)
                          (lambda (dummy) expr)])])
      (let-syntax ([force (syntax-rules ()
                            [(force expr)
                             (expr 0)])])
        (force (delay 7))))))
  (list
   (blank)
   (para #:width WIDE "In Racket:")
   (blank)
   (sscode
    (define-syntax delay (syntax-rules ()
                           [(delay expr)
                            (lambda (dummy) expr)]))
    code:blank
    (define-syntax force (syntax-rules ()
                           [(force expr)
                            (expr 0)]))
    code:blank
    (force (delay 7))))
  (list
   (blank)
   (para #:width WIDE "In Racket:")
   (blank)
   (sscode
    (define-syntax-rule (delay expr)
      (lambda (dummy) expr))
    code:blank
    (define-syntax-rule (force expr)
      (expr 0))
    code:blank
    (force (delay 7))))))


;; ----------------------------------------
(part 2)

(simple-pattern-slides)

;; ----------------------------------------
(part 3)
(pattern-slides)

;; ----------------------------------------
(part 4)
(pattern-...-slides)

;; ----------------------------------------
(part 5)

(lexical-slides)
;; (lexical-how-slides) ;; <--- included in old version

;; ----------------------------------------
(part 6)

(id-macro-slides)

;; ----------------------------------------
(part 7)

(macro-generating-macro-slides)

;; ----------------------------------------
(part 8)

(cbr-slides)

;; ----------------------------------------
(part 9)

(define call-f
  (code
   (let ([a 0] [b 1])
     (f a b))))

(define (make-cbr+f #:export? [export? #f]
                    #:rename? [rename? #f])
  (ht-append
   gap-size
   (in-file
    "cbr.rkt"
    (code #,(if rename?
                (code (provide 
                       (rename-out
                        [define-cbr define])))
                (code (provide define-cbr)))
          code:blank
          ....))
   (in-file
    "f.rkt"
    (code (require "cbr.rkt")
          code:blank
          #,(if rename?
                (code
                 (define (f x y)
                   ....))
                (code
                 (define-cbr (f x y)
                   ....)))
          code:blank
          #,(if export?
                (code (provide f))
                call-f)))))

(slide
 #:title "Modules"
 #:layout 'top
 (para #:width (* client-w 0.9)
       "Modules can export and import macros")
 (blank)
 'alts
 (list
  (list (make-cbr+f))
  (list
   (vr-append
    gap-size
    (make-cbr+f #:export? #t)
    (in-file "g.rkt"
             (code (require "f.rkt")
                   code:blank
                   #,call-f))))))

(slide
 #:title "Renaming Exports"
 #:layout 'top
 (para #:width (* client-w 0.9)
       "The" (code provide) "form supports renaming")
 (blank)
 (make-cbr+f #:rename? #t))

(slide
 #:title (hbl-append (titlet "Modules and ") (tt "#lang"))
 (hc-append
  gap-size
  (in-file
   "f.rkt"
   (code #,(tt "#lang racket")
         code:blank
         (define (f x y)
           ....)))
  (tt "=")
  (in-file
   "f.rkt"
   (code (module f racket
           (define (f x y)
             ....))))))

(slide
 #:title "Adjusting a Language"
 (para #:width (* client-w 0.9)
       "The" (code provide) "form has its own sublanguage...")
 (in-file
  "racket-cbr.rkt"
  (code #,(tt "#lang racket")
        code:blank
        (provide (except-out (all-from-out racket)
                             define)
                 (rename-out [define-cbr define]))
        code:blank
        ...))
 'alts
 (list
  (list
   (in-file
    "f.rkt"
    (code (module f "racket-cbr.rkt"
            (define (f x y)
              ....)))))
  (list
   (in-file
    "f.rkt"
    (code #,(tt "#lang s-exp \"racket-cbr.rkt\"")
          code:blank
          (define (f x y)
            ....))))))

#|
;; ----------------------------------------
(part 10)

(parsing-slides)

(define (exps a sep b)
  (vl-append (current-line-sep)
             a
             (t sep)
             b))

(slide
 #:title "Parsing in Racket"
 (para (exps (tt "#lang racket") "⇒" (code (module _name racket ....))))
 (blank)
 (blank)
 (para (exps (code #,(tt "#lang s-exp") _path) "⇒" (code (module _name _path ....)))))
|#
