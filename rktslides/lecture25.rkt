#lang slideshow
(require slideshow/code
         slideshow/step
         "utils/utils.rkt"
         "utils/in-file.rkt")

(define (transformer op stx? src dest 
                     #:arrow? [arrow? #t]
                     #:extra [extra #f])
  (define p (inset (apply
                    vl-append
                    3
                    src
                    ((if arrow? values ghost)
                     (hbl-append (colorize (bt " → ") "blue")
                                 dest))
                    (if extra
                        (list extra)
                        null))
                   12))
  (define b
    (cc-superimpose 
     (colorize (filled-rectangle (pict-width p)
                                 (pict-height p))
               "lightblue")
     (frame p
            #:line-width 1
            #:color "black")))
  (define bx (if extra
                 (let* ([bx (transformer #f stx? src dest)]
                        [bx (inset bx 0 0 (max 0 (- (pict-width b) (pict-width bx))) 0)])
                   (refocus (lt-superimpose (ghost bx)
                                            b)
                            bx))
                 b))
  (refocus
   (vl-append (current-line-sep)
              (if op
                  (scale (cond
                          [(not (string? op)) op]
                          [stx?
                           (let ([pre (tt op)]
                                 [post (tt ".rkt")])
                             (hbl-append
                              (refocus (hbl-append 
                                        pre
                                        (inset (colorize
                                                (text "-loc"
                                                      (current-code-font)
                                                      (current-font-size)
                                                      (* pi 1/4))
                                                "red")
                                               -12 0 0 0))
                                       pre)
                              post))]
                          [else
                           (tt (~a op ".rkt"))])
                         0.75)
                  (blank))
              bx)
   bx))

(define (step arrow? l r)
  (define h (/ (pict-height l) 2))
  (ht-append
   (hb-append (hb-append
               l
               (let ([p (blank gap-size h)])
                 (if arrow?
                     (pin-arrow-line
                      (/ gap-size 2)
                      p p lc-find p rc-find
                      #:color "blue"
                      #:line-width 3)
                     p))))
   (inset r 0 h 0 0)))

(define (steps-slide title stx? parse? d->s? t->i? ->i? unparse?)
  (define (v ?) (if ? values ghost))
  (slide
   #:title title
   (step
    d->s?
    ((v d->s?)
     (transformer #f stx?
                  (code #'code:blank)
                  (code 'code:blank)))
    (step
     parse?
     ((v parse?)
      (transformer "typed-parse" stx?
                   (if stx?
                       (code #'code:blank)
                       (code 'code:blank))
                   (code ClassT)))
     (step
      ->i?
      (transformer "typed-class" stx?
                   (code ClassT)
                   ((v t->i?) (code ClassI))
                   #:arrow? t->i?
                   #:extra (code typecheck))
      (step
       unparse?
       ((v ->i?)
        (transformer "inherit" stx?
                     (code ClassI)
                     (code Class)))
       ((v unparse?)
        (transformer "unparse" stx?
                     (code Class)
                     (if stx?
                         (code #'code:blank)
                         (code 'code:blank))))))))))

;; ----------------------------------------
(part 1)
(define typed-s-exp-title "Typed Classes")

(void
 (with-steps~ 
  (typecheck parse d->s t->i ->i unparse)
  (steps-slide typed-s-exp-title
               #f
               (after? parse)
               (after? d->s)
               (after? t->i)
               (after? ->i)
               (after? unparse))))

;; ----------------------------------------
(part 2)
(define typed-stx-title "Typed Classes with Source Locations")

(steps-slide typed-stx-title #f #t #t #t #t #t)
(steps-slide typed-stx-title #t #t #f #t #t #t)

;; ----------------------------------------
(part 3)

(slide
 #:title (tt "#lang typed-class")
 (para "Right now, we have to write")
 (in-file "typed-loc-posn.rkt"
          (code #,(tt "#lang s-exp") "typed-class-lang.rkt"
                ....))
 'next
 (blank)
 (blank)
 (para "We'd prefer to write")
 (in-file "posn.rkt"
          (code #,(tt "#lang typed-class")
                ....)))
 
(define languages-title "Languages")

(slide
 #:title languages-title
 (vc-append
  (current-line-sep)
  (para "A name like" (code typed-class) "is used"
        "as a" (bit "collection") "name")
  (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
    (para #:align 'right
          "... and the collection's" (tt "\"main.rkt\"") "module is used")))
 'next
 (blank)
 (para "In the simple case, a" (bit "package") "implements a collection")
 'next
 (blank)
 (para "In the simplest case, a" (bit "directory") "implements a package"))

(define (language-slide slash? read? color?)
  (slide
   #:title languages-title
   (code #,(tt "#lang") _module-path)
   (blank)
   'next
   (blank)
   (vc-append
    (current-line-sep)
    (item "Find" (code _module-path))
    ((if slash? values ghost)
     (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
       (para #:align 'right
             "... adding" (tt "/main") "if no" (tt "/")))))
   'next
   (blank)
   (vc-append
    (current-line-sep)
    (item "Look for a" (code reader) "submodule")
    (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))])
      (cond
       [color?
        (para #:align 'right
              "... which provides a" (code get-info) "function"
              "to configure details such as syntax coloring")]
       [read?
        (para #:align 'right
              "... which parses characters into" (code (module ....))
              "by providing a" (code read-syntax) "function")]
       [else
        (para #:align 'right
              "... to control" (code #,(tt "#lang") _module-path)
              "expansion to" (code (module _name _initial-import ....)))])))))

(language-slide #t #f #f)

;; ----------------------------------------
(part 4)

(define (parax s p)
  (ht-append gap-size
             (inset (para #:width (* client-w 1/3) #:fill? #f s)
                    0 gap-size 0 0)
             (scale p 0.85)))

(slide
 #:title (tt "#lang typed-class")
 (parax "Right now, we write"
        (in-file "posn.rkt"
                 (scale
                  (code #,(tt "#lang typed-class")
                        ....
                        {class (code:line posn3D extends posn)
                          {[z : num]}
                          {mdist : num -> num
                                 {+ {get this z}
                                    {super mdist arg}}}}
                        ....)
                  0.75)))
 'next
 (blank)
 (parax "Conceivably, someone might prefer to write"
        (in-file "infix-posn.rkt"
                 (scale
                  (vl-append
                   (current-line-sep)
                   (tt "#lang typed-class/infix")
                   (tt "...")
                   (tt "class posn3D extends posn {")
                   (tt "  num z;")
                   (tt "  num mdist(num arg) {")
                   (tt "    return this.z + super.mdist(arg);")
                   (tt "  }")
                   (tt "}")
                   (tt "..."))
                  0.75))))

(slide
 #:title "Parsing Characters"
 (item "Is" (tt "posn3D") "an identifier? Is" (tt "3Dposn") "an identifier?")
 (blank)
 'next
 'alts
 (list
  (list
   (vc-append
    (current-line-sep)
    (item "Is")
    (tt "1+2*3")
    (item #:bullet (ghost bullet) "the same as")
    (tt "(1+2)*3")
    (item #:bullet (ghost bullet) "or")
    (tt "1+(2*3)")))
  (list
   (vc-append
    (current-line-sep)
    (item "Is")
    (tt "this.m(0) + this.n(1)")
    (item #:bullet (ghost bullet) "the same as")
    (tt "(this.m(0)) + (this.n(1))")
    (item #:bullet (ghost bullet) "or")
    (tt "(this.m(0) + this).n(1)"))))
 'next
 (blank)
 (item "Is" (tt "class") "a reserved word?")
 'next
 (blank)
 (item "Is" (tt "1+2") "the same as" (tt "1 + 2") "?"))

(slide
 #:title "Lexing and Parsing"
 (para "Characters:")
 (parameterize ([code-colorize-enabled #f])
   (code c l a s s #,(scale (t "‹space›") 0.85) p o s n 3 D #,(t "...") #,(t "{") #,(t "...")))
 'next
 (para "Tokens:")
 (code CLASS WHITESPACE posn3D #,(t "...") OPENB #,(t "..."))
 'next
 (blank)
 (blank)
 'alts
 (let ([mk
        (lambda (abs? reader?)
          (step
           #t
           (transformer "lexer" #f
                        (t "characters")
                        (t "tokens"))
           (step
            reader?
            (transformer "parser" #f
                         (t "tokens")
                         (if abs?
                             (t "abstract syntax")
                             (code #'code:blank)))
            (if abs?
                (blank)
                ((if reader? values ghost)
                 (transformer "reader" #f
                              (code #'code:blank)
                              (code #'(module ....))))))))])
   (list (list (lt-superimpose (mk #t #f)
                               (ghost (mk #f #f))))
         (list (mk #f #f))
         (list (mk #f #t)))))

;; ----------------------------------------
(part 5)

(language-slide #f #t #f)

;; ----------------------------------------
(part 6)

(slide
 #:title "Syntax Coloring"
 'alts
 (list
  (list
   (vl-append
    (current-line-sep)
    (colorize (tt "#lang typed-class/infix0") "red")
    (tt "....")
    (tt "(new posn3D(5, 3, 1)).addDist(new posn(2, 7));")))
  (list
   (vl-append
    (current-line-sep)
    (tt "#lang typed-class/infix")
    (tt "....")
    (let ([mk (lambda (col)
                (lambda (s) (colorize (tt s) col)))])
      (define brown (mk "brown"))
      (define blue (mk "darkblue"))
      (define green (mk "forestgreen"))
      (define black tt)
      (hbl-append (brown "(")
                  (blue "new posn3D")
                  (brown "(")
                  (green "5")
                  (black ",")
                  (green " 3")
                  (black ",")
                  (green " 1")
                  (brown "))")
                  (black ".")
                  (blue "addDist")
                  (brown "(")
                  (blue "new posn")
                  (brown "(")
                  (green "2")
                  (black ",")
                  (green " 7")
                  (brown "))")
                  (black ";"))))))
 'next
 (blank)
 (blank)
 (blank)
 (step
  #t
  (transformer "lexer" #f
               (t "characters")
               (t "tokens"))
  (step
   #t
   (transformer "colorer" #f
                (t "tokens")
                (t "color"))
   (transformer (t "DrRacket") #f
                (t "color")
                (t "image")))))

(language-slide #f #f #t)
