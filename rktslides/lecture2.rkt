#lang slideshow
(require slideshow/code
         "utils/utils.rkt"
         "utils/arith.rkt")

(fullscreen-para-width!)

(define step-arrow
  (let ([p (t "→")])
    (lbl-superimpose p (inset p (/ (pict-width p) 3) 0 0 0))))

;; ----------------------------------------

(slide (titlet "Part 1"))

(slide
 (para "An" (bit "interpreter") "takes a program and returns it value")
 'next
 (blank)
 (blank)
 (para (bit "Plait") "= the language that we use to write interpreters")
 (blank)
 (para (bit "Curly") "= the language that to be interpreted")
 (para #:align 'right "... that keeps changing"))

(slide
 #:title "Curly Arithmetic"
 'alts
 (list
  (list
   (code {+ 2 1})
   'next
   (code #,step-arrow 3))
  (list
   (code {* 2 1})
   'next
   (code #,step-arrow 2))
  (list
   (code {+ 2 {* 4 3}})
   'next
   (code #,step-arrow 14))
  (list
   (code 2)
   'next
   (code #,step-arrow 2))))

(slide
 #:title "Representing Expressions"
 (code
  2
  code:blank
  {+ 2 1}
  code:blank
  {+ 2 {* 4 3}})
 'next
 (blank)
 'alts
 (list
  (get-expression-inventory)
  (list
   (code
    (define-type Exp
      (numE [n : Number])
      (plusE [l : Exp]
             [r : Exp])
      (multE [l : Exp]
             [r : Exp]))))))

;; ----------------------------------------

(slide (titlet "Part 2"))

(slide
 #:title "Curly Interpeter"
 (scode
  (define (interp [a : Exp]) : Number
    (type-case Exp a
      [(numE n) n]
      [(plusE l r) (+ (interp l) (interp r))]
      [(multE l r) (* (interp l) (interp r))]))
  code:blank
  (test (interp (numE 2))
        2)
  (test (interp (plusE (numE 2) (numE 1)))
        3)
  (test (interp (multE (numE 2) (numE 1)))
        2)
  (test (interp (plusE (multE (numE 2) (numE 3))
                       (plusE (numE 5) (numE 8))))
        19)))

;; ----------------------------------------

(slide (titlet "Part 3"))

(slide
 #:title "Concrete vs. Abstract Syntax"
 (code {+ 2 1})
 'next
 (blank)
 (code (plusE (numE 2) (numE 1))))
 
(slide
 #:title "Concrete Syntax as an S-Expression"
 (code `{+ 2 1})
 'next
 (blank)
 (code (test (parse `{+ 2 1})
             (plusE (numE 2) (numE 1)))))

(define v-arrow (arrow gap-size (* pi 3/2)))

(define (parse-explain show-dt show-parse)
  (slide
   #:title "Concrete Syntax as an S-Expression"
   (code
    (code:comment "An EXP is either")
    (code:comment " - `NUMBER")
    (code:comment " - `{+ EXP EXP}")
    (code:comment " - `{* EXP EXP}"))
   (blank)
   (show-parse
    (vc-append
     v-arrow
     (code parse)
     v-arrow))
   (blank)
   (show-dt
    (code
     (define-type Exp
       (numE [n : Number])
       (plusE [l : Exp] [r : Exp])
       (multE [l : Exp] [r : Exp]))))))

(parse-explain ghost ghost)
(parse-explain values ghost)
(parse-explain values values)

(slide
 #:title "Matching an S-Expression"
 (code
  (code:comment "An EXP is either ...")
  (code:comment " - `{* EXP EXP}"))
 'next
 (blank gap-size)
 'alts
 (let* ([big-body
         (code
          (and (s-exp-list? s) 
               (= 3 (length (s-exp->list s)))
               (s-exp-symbol? (first (s-exp->list s)))
               (eq? '* (s-exp->symbol (first (s-exp->list s)))))
          code:blank)]
        [match (code (s-exp-match? `{* ANY ANY} s))]
        [mk-parse
         (lambda (body)
           (code
            (define (parse [s : S-Exp]) : Exp
              ....
              #,(lt-superimpose
                 (ghost big-body)
                 body)
              ....)))]
        [mk-cond
         (lambda (show)
           (let ([g (ghost match)])
             (pin-over
              (show
               (code (cond
                       ....
                       [#,g
                        .... (parse (second (s-exp->list s)))
                        .... (parse (third (s-exp->list s))) ....]
                       ....)))
              g lt-find
              match)))])
   (list
    (list (mk-parse (code code:blank)))
    (list (mk-parse big-body))
    (list (mk-parse (mk-cond ghost)))
    (list (mk-parse (mk-cond values))))))

;; ----------------------------------------

#;
(begin
  (slide (titlet "Part 3"))

  (slide
   #:title "Subtraction"
   'alts
   (list
    (list
     (code {- 5 2})
     'next
     (code #,step-arrow 3))
    (list
     (questionable
      (code
       (define-type Exp
         (numE [n : Number])
         (plusE [l : Exp] [r : Exp])
         (minusE [l : Exp] [r : Exp]) #,new-label
         (multE [l : Exp] [r : Exp])))))
    (list
     (code {- 5 2})
     'next
     (code = {+ 5 -2}))
    (list
     (code {- 5 2})
     'next
     (code = {+ 5 {* -1 2}}))))

  (slide
   #:title "Desugaring"
   (code
    (define-type Exp
      (numE [n : Number])
      (plusE [l : Exp] [r : Exp])
      (multE [l : Exp] [r : Exp]))
    code:blank
    (define-type Sugar-Exp (code:comment "not Exp")
      (numSE [n : Number])
      (plusSE [l : Sugar-Exp] [r : Sugar-Exp])
      (minusSE [l : Sugar-Exp] [r : Sugar-Exp])
      (multSE [l : Sugar-Exp] [r : Sugar-Exp]))
    code:blank
    desugar : (Sugar-Exp -> Exp))))

