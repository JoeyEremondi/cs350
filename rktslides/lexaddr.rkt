#lang slideshow
(require slideshow/code
         slideshow/balloon
         "utils.ss"
         "colors.ss")

(provide mk-addr-slide)

(define ans (colorize (bt "Answer:") BlueColor))

(define (mk-addr-slide orig-expr inner-expr what where shape-balloon)
  (slide
   #:title "Identifier Address"
   (para "Suppose that")
   (cc-superimpose orig-expr
                   (code code:blank
                         code:blank
                         code:blank))
   (para "appears in a program; the body is eventually evaluated:")
   (blank)
   inner-expr
   (para (it "where") "will" what "be in the environment?")
   'next
   (blank)
   (para ans "always" where ":")
   (balloon-pict shape-balloon)))

