#lang slideshow
(require slideshow/code
         "utils/alg.rkt"
         "utils/copy.rkt"
         "utils/utils.rkt"
         "utils/colors.rkt"
         "gear.rkt")

(define NoteColor "brown")

(define my-line-sep (current-line-sep))

(slide
 (vc-append
  my-line-sep
  (titlet "CS 3520/6520")
  (titlet "Programming Languages"))
 (blank)
 (scale/improve-new-text (t "Fall 2020") 0.75)
 (blank)
 (blank)
 (hbl-append
  (t "Instructor: ")
  (colorize (bt "Matthew Flatt") NoteColor))
 (blank)
 (htl-append
  (t "TAs: ")
  (vl-append (current-line-sep)
             (colorize (bt "Brandon Mouser") NoteColor)
             (colorize (bt "Kyle Price") NoteColor))))

(define (emph s)
  (colorize (bt s) NoteColor))

(define cs3520-title "CS 3520/6520 Programming Languages")

(define (xt s) (scale (t s) 1.2))

(define (survey-slide #:reveal? [reveal? #f])
  (slide
   #:title cs3520-title
   (para ((if reveal? values ghost) (colorize (bt "Not") "red")) (colorize (bt "a survey course:") "firebrick"))
   (cellophane
    (table
     2
     (let ([s 0.8])
       (list (scale (bitmap "logos/smalltalk-logo.png") (* s 0.5)) (xt "an object-oriented language")
             (scale (bitmap "logos/haskell-logo.png") (* s 0.25)) (xt "a functional language")
             (scale (bitmap "logos/swi-prolog.png") (* s 0.3)) (xt "a logic language")))
     (list rc-superimpose lc-superimpose)
     cc-superimpose
     (* 2 gap-size) (* 2 gap-size))
    (if reveal? 0.3 1.0))))
(survey-slide)
(survey-slide #:reveal? #t)

(slide
 #:title cs3520-title
 (para "This course is about programming language" (emph "concepts"))
 (blank)
 'next
 'alts
 (list
  (list
   (table 3
          (let ([half (lambda (s)
                        (define p (t s))
                        (inset p 0 0 (* -1/2 (pict-width p)) 0))])
            (map (lambda (s) (if (string? s) (t s) s))
                 (list
                  "lexical scope" "closures" "recursion"
                  "λ-calculus" "objects" "classes"
                  "continuations" (half "eager and lazy evaluation") (blank)
                  "state" "type checking" "polymorphism"
                  "soundness" "type inference" "subtyping"
                  "compilation" (half "garbage collection") (blank))))
          lc-superimpose lc-superimpose
          (* 3 gap-size) gap-size)
   'next
   (blank)
   (para #:align 'right
         "... especially" (emph "functional programming") "concepts")
   'next
   (blank)
   (it "use one language, implement many languages"))
  (parameterize ([current-para-width (* 0.9 (current-para-width))])
    (list
     (blank)
     (item "To help you understand new programming languages")
     (blank)
     'alts~
     (let ([p (t "To make you a better programmer in any language")])
       (list
        (list (item p))
        (list (item (let ([c (let ([p (inset p 4)]) (filled-rounded-rectangle (pict-width p) (pict-height p)))])
                      (refocus (cc-superimpose (colorize c "gold")
                                               p)
                               p))))))))))

(slide
 #:title "Course Details"
 (tt "http://www.eng.utah.edu/~cs3520/")
 'next
 (blank gap-size)
 (vl-append
  gap-size
  (para #:fill? #f "Formal prerequisite: CS 3500")
  (para #:fill? #f "Informal prerequisite: more programming experience than that"))
 'next
 (blank gap-size)
 (vl-append
  gap-size
  (item #:fill? #f "Homework is weekly, roughly")
  (item #:fill? #f "Two mid-term exams")
  (item #:fill? #f "Extended homework assignment place of a final exam")
  (item #:fill? #f "Late policy: up to 48 hours, two automatic ``free lates''")))

(slide
 #:title "Lectures are Online"
 (para "All slide presentations are online")
 'next
 (blank)
 (item (bt "Watch the videos before class"))
 'next
 (blank)
 (vc-append
  (current-line-sep)
  (item (bt "Meet as a class for more examples and homework solutions"))
  (subitem "a.k.a. ``recitation''")
  (subitem "guideline: no new material introduced in class")
  (subitem "need volunteers to sign up"))
 'next
 (blank)
 (vc-append
  (current-line-sep)
  (item (bt "Volunteer for in-class participation"))
  (subitem "be prepared to share your screen, write code with class help")
  (subitem "see sign-up in Canvas")
  (subitem "presenters get one extra ``free late''")))

(define blank-bullet (ghost bullet))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (aside . l)
  (scale/improve-new-text
   (apply para #:align 'center #:width (* 0.9 client-w) l)
   0.9))

(define (cbt s) (colorize (if (pict? s) s (bt s)) "forestgreen"))

(define vm-icon
  (let ([p (inset
            (vr-append
             -40
             (inset
              (ht-append -5
                         (gear #:n 7 #:rotate (* pi 1/8))
                         (scale (gear #:n 5 #:rotate (* pi 1/6)) 0.7))
              0 0 10 0)
             (scale (gear #:n 5 #:rotate (* pi 0)) 0.5))
            10)])
    (inset (cc-superimpose
            (colorize (filled-rounded-rectangle (pict-width p) (pict-height p))
                      "indigo")
            p)
           10)))

(define into (colorize (arrow gap-size 0) "forestgreen"))

(slide
 #:title "Interpreters"
 (blank)
 'alts
 (list
  (list
   (item (bt "Learn concepts by implementing") (colorize (bt "interpreters") NoteColor))
   (blank)
   (hc-append gap-size
              (let ([s (* 3 gap-size)])
                (cc-superimpose (file-icon s (* 1.25 s) "beige")
                                (tt "1+2")))
              into
              vm-icon
              into
              (tt "3"))
   (blank)
   'next
   (para #:align 'center "new concept" sym:implies "new interpreter")
   'next
   (blank)
   (item #:bullet (ghost bullet) "We'll always call the language that we implement" (cbt "Curly")
         ", even though the language keeps changing"))
  #;
  (list
   (para "An" (emph "interpreter") "takes a program and produces a result")
   (vl-append
    my-line-sep
    (subitem "JavaScript engine in your browser")
    (subitem (tt "/bin/bash"))
    (subitem "desktop calculator")
    (subitem "x86 processor")
    (subitem "Algebra student")
    (subitem "DrRacket"))
   'next
   (blank)
   (para "A" (emph "compiler") "takes a program and produces another program")
   (blank)
   (blank)
   (aside "In the terminology of programming languages, someone who translates"
          "Chinese to English is a" (it "compiler") ", not an" (it "interpreter")"."))))
(start-at-recent-slide)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begat-arrow a) (inset (arrow (/ (current-font-size) 2) a) (/ gap-size 2) 0))

(define begat (inset (t "⇒") (/ gap-size 2) 0))
(define tageb (let ([w (pict-width begat)])
                (inset (scale begat -1 1) (- w 0) 0 w 0)))
(define (ebt s) (colorize (if (pict? s) s (bt s)) "darkblue"))
(define (mbt s) (colorize (if (pict? s) s (bt s)) NoteColor))

(slide
 #:title "Racket and Plait"
 (let ([p (parameterize ([current-main-font (cons 'bold (current-main-font))])
            (para "Implement interpreters using" (mbt "Plait") ", a variant of" (mbt "Racket")))])
   (refocus (item p) p))
 'next
 (blank)
 'alts
 (let ([l (list "Historically:" (ebt "Lisp") begat (ebt "Scheme") begat (mbt "Racket") begat (mbt "Plait"))])
   (list
    (list (apply para l))
    (list (apply para (append l (list tageb (ebt "ML")))))))
 'next
 (blank)
 (let ([p (vl-append
           gap-size
           (para (mbt "Racket") "is")
           (item "a programming language")
           (item "a family of programming languages")
           (item "a language for creating programming languages"))])
   (rc-superimpose p (scale (bitmap "racket.png") 0.5)))
 'next
 (para #:align 'center "... including" (mbt "Plait"))
 'next
 (blank)
 (parameterize ([current-font-size (floor (* #e0.7 (current-font-size)))])
   (para #:align 'right
         "PLAI =" (it "Programming Languages: Application and Interpretation") ", a textbook")))

(slide
 #:title "DrRacket"
 (scale (bitmap "drracket-plait.png") 0.6))

(define (preview s)
  (format "Preview: ~a" s))

(slide
 #:title (preview "Plait Tutorial")
 (tt "http://docs.racket-lang.org/plait/index.html")
 (blank (* 2 gap-size))
 (scale (bitmap "plait-docs.png") 0.6))

(slide
 #:title (preview "Plait's Parenthesized Prefix Notation")
 (table 2
        (list (alg-code "f(x)")       (code (f x))
              (alg-code "1+2")        (code (+ 1 2))
              (alg-code "1+2*3")      (code (+ 1 (* 2 3)))
              (alg-code "s=6")        (code (define s 6))
              (alg-code "f(x)=x+1")   (code (define (f x)
                                              (+ x 1)))
              (let ([p (table 2
                              (list (alg-code "x<0") (alg-code "-1")
                                    (alg-code "x=0") (alg-code "0")
                                    (alg-code "x>0") (alg-code "1"))
                              (list ltl-superimpose rtl-superimpose) ltl-superimpose
                              gap-size (current-line-sep))]
                    [b (rt "{")])
                (hc-append (scale b (* 1.2 (/ (pict-height p) (pict-height b))))
                           p))
              (code (cond
                      [(< x 0) -1]
                      [(= x 0) 0]
                      [(> x 0) 1])))
        ltl-superimpose ltl-superimpose
        (* 3 gap-size) (* 1.5 gap-size)))

(define (data-item kind comment)
  (rtl-superimpose
   (item kind)
   (colorize (t comment) "blue")))

(slide
 #:title (preview "Plait Data")
 (data-item "Numbers and strings" "obvious")
 (code 1 code:blank 3.4 code:blank "Hello, World!")
 'next
 (blank gap-size)
 (data-item "Booleans" "straightforward")
 (code #t code:blank #f)
 'next
 (blank gap-size)
 (data-item "Symbols and quoted lists" "unusual")
 (code 'apple code:blank 'define code:blank '+)
 (blank)
 (code '(1 2 3) code:blank '(f x)))

(slide
 #:title (preview "Plait S-Expressions")
 (data-item (para #:fill? #f "Backquote" (tt "`") "instead of regular quote" (tt "'"))
            "convenient")
 (blank gap-size)
 (code `x)
 (blank gap-size)
 (code `{+ x 1})
 (blank gap-size)
 (code `{define {f x}
          {+ x 1}}))

(slide
 #:title (preview "Plait Datatypes")
 (add-copy
  code
  (define-type Shape
    (circle [radius : Number])
    (rectangle [width : Number]
               [height : Number]))
  code:blank
  (define (area s)
    (type-case Shape s
      [(circle r) (* 3.14 (* r r))]
      [(rectangle w h) (* w h)]))
  code:blank
  (test (area (circle 2))
        12.56)
  (test (area (rectangle 3 4))
        12)))

(slide
 #:title (preview "Interpreters")
 (para #:fill? #f "See" (tt "lambda.rkt"))
 'next
 (para "Example" (mbt "Plait") "program:")
 (code (define-type Value
         (numV [n : Number])
         (closV [arg : Symbol]
                [body : Exp]
                [env : Env])))
 'next
 (para "Example" (cbt "Curly") "program:")
 (code {+ {* 3 4} 8})
 'next
 (para "Example" (cbt "Curly") "program as a" (mbt "Plait") "value:")
 (code `{+ {* 3 4} 8}))


;; ----------------------------------------

(define shape+-datatype
  (code
   (define-type Shape
     (circle [radius : Number])
     (rectangle [width : Number]
                [height : Number])
     (adjacent [left : Shape]
               [right : Shape]))))

(define area+-defn
  (code
   (define (area s)
     (type-case Shape s
       [(circle r) (* 3.14 (* r r))]
       [(rectangle w h) (* w h)]
       [(adjacent l r) (+ (area l)
                          (area r))]))))

(define (add-arrows p on? . specs)
  (let loop ([p p] [specs (if on? specs '())] [step 0])
    (cond
      [(null? specs) p]
      [else
       (loop
        (pin-arrow-line gap-size
                        p
                        p (lambda args
                            (values (* (cadr specs) (pict-width p))
                                    (* (car specs) (pict-height p))))
                        p (lambda args
                            (values (* (cadddr specs) (pict-width p))
                                    (* (caddr specs) (pict-height p))))
                        #:start-angle (* 1/8 pi)
                        #:end-angle (- (* 10/8 pi) step)
                        #:start-pull 1/2
                        #:end-pull 1/2
                        #:color "orange"
                        #:line-width 3)
        (list-tail specs 4)
        (- step 0.1))])))
       

(define (match-slide #:function-arrows? [function-arrows? #f]
                     #:datatype-arrows? [datatype-arrows? function-arrows?])
  (slide
   #:title "Datatype and Function Shapes Match"
   (scale
    (code
     #,(add-arrows shape+-datatype
                   datatype-arrows?
                   45/60 3/4    2/60 10/20
                   55/60 32/40  2/60 9/20)
     code:blank
     #,(add-arrows area+-defn
                   function-arrows?
                   45/60 37/50  2/60 11/30
                   55/60 39/50  2/60 9/30)
     code:blank
     (test (area (circle 2))
           12.56)
     (test (area (rectangle 3 4))
           12)
     (test (area (adjacent (circle 2) (rectangle 3 4)))
           24.56))
    0.8)))

(match-slide)
(match-slide #:datatype-arrows? #t)
(match-slide #:function-arrows? #t)

;; ----------------------------------------

(define (span n lbl)
  (define p (t lbl))
  (ht-append gap-size
             (colorize (filled-rectangle 10 (* n (pict-height p)))
                       "blue")
             p))

(slide
 #:title "Course Outline"
 (scale
  (vl-append
   gap-size
   (span 1 "Functional programming")
   (span 2 "Interpreters")
   (span 1.5 "State")
   (span 2.5 "Control")
   (span 1 "Compilation and GC")
   (span 1 "Objects and classes")
   (span 3 "Types")
   (span 1 "Macros"))
  0.9))

;; ----------------------------------------

(slide
 #:title "Homework 0"
 (vl-append
  gap-size
  (item #:fill? #f "Create handin account")
  (item #:fill? #f "Plait warm-up exercises"))
 (blank)
 (colorize
  (para #:align 'center "Due Sunday,\xA0August 30")
  NoteColor))
