#lang slideshow
(require slideshow/code
         slideshow/balloon
         "utils/utils.ss"
         "utils/colors.ss"
         "utils/alg.ss"
         "utils/gcpict.ss")

;; ----------------------------------------
(part 1)

(define-syntax sshilite
  (syntax-rules ()
    [(_ expr)
     (colorize (parameterize ([code-colorize-enabled #f])
                 (un-ss-scale (sscode expr)))
               RedColor)]))

(slide
 #:title "Allocation"
 (para #:width (get-client-w #:aspect 'fullscreen)
       "Constructor calls are allocation:")
 'alts
 (list
  (list
   (scale
    (sscode
     (define (interp)
       (type-case ExpD expr-reg
         ....
         [(lamD body-expr)
          (begin
            (set! v-reg (#,(sshilite closV) body-expr env-reg))
            (continue))]
         ....))
     code:blank
     (define (continue)
       ....
       [(plusSecondK r env k)
        (begin
          (set! expr-reg r)
          (set! env-reg sc)
          (set! k-reg (#,(sshilite doPlusK) v-reg k))
          (interp))]
       ....))
    1.0))
  #;
  (list
   (sscode
    (define (interp)
      (case (ref expr-reg 0)
        ....
        [(code:line (12) (code:comment "lam"))
         (begin
           (set! v-reg (malloc2 16 (ref expr-reg 1) env-reg))
           (continue))]
        ....))
    code:blank
    (define (continue)
      ....
      [(code:line (1) (code:comment "plusSecondK"))
       (begin
         (set! expr-reg (ref k-reg 1))
         (set! env-reg (ref k-reg 2))
         (set! k-reg (malloc2 2 v-reg (ref k-reg 3)))
         (interp))]
      ....)))))
      
(define do-add-k-code
  (scode
   [(doPlusK v1 k)
    (begin
      (set! v-reg (num+ v1 v-reg))
      #,(sshilite (free k-reg)) (code:comment "???")
      (set! k-reg k)
      (continue))]))

(define do-app-k-code
  (scode
   [(doAppK fun-val k)
    (begin
      (set! expr-reg (closV-body fun-val))
      (set! env-reg (cons v-reg
                          (closV-env fun-val)))
      (set! k-reg k)
      #,(sshilite (free fun-val)) (code:comment "???")
      (interp))]))

(slide
 #:title "Deallocation"
 'alts
 (list
  (list
   (para #:width (get-client-w #:aspect 'fullscreen)
         "Where does" (code free) "go?")
   (sscode
    (define (continue)
      ....
      #,(un-s-scale do-add-k-code)
      ....
      #,(un-s-scale do-app-k-code)
      ....)))
  (list
   do-add-k-code
   'next
   (blank)
   (item "Without" (code let/cc) ", this free is fine, because the"
         "continuation can't be referenced anywhere else")
   (item "A continuation object is always freed as" (code (free k-reg))
         ", which is why many language implementations use a stack"))
  (list
   do-app-k-code
   'next
   (blank)
   (item "This free is" (it "not") "ok, because"
         "the closure might be kept in a environment somewhere")
   (item "Need to free only if no one else is using it..."))))

(slide
 #:title "Code and Data"
 (para "An" (bit "object") "is any record allocated during"
       (code interp) "and" (code continue))
 'next
 (blank)
 (para "Assume that expressions are allocated ``statically''")
 'next
 (item (code compile) "uses" (code code-malloc1) ", etc.")
 (item "Only try to free objects allocated during"
       (code interp) "and" (code continue)))
 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 2)

(slide
 #:title "Reference Counting"
 (para (dt "Reference counting:")
       "a way to know whether an object has other users")
 'next
 (blank)
 (item "Attatch a count to every object, starting at 0")
 'next
 (item "When installing a pointer to an object"
       "(into a register or another object),"
       "increment its count")
 (item "When replacing a pointer to an object,"
       "decrement its count")
 'next
 (item "When a count is decremented to 0, decrement"
       "counts for other objects referenced by the object,"
       "then free"))

(define ann-w (* (get-client-w #:aspect 'fullscreen) 0.45))

(define (ann-item . l)
  (apply para #:width ann-w l))

(define (make-gc-picture/ann a b c d . ann)
  (let ([w ann-w])
    (ht-append
     (cc-superimpose
      (blank w 0)
      (make-gc-picture a b c d))
     (cc-superimpose
      (blank w 0)
      (apply
       vl-append
       (current-font-size)
       ann)))))

(slide
 #:title "Reference Counting"
 'alts
 (list (list (make-gc-picture/ann
              `(rc) `() null #f
              (ann-item "Top boxes are the registers"
                        (code k-reg) "," (code v-reg) ", etc." )
              (ann-item "Boxes in the blue area are allocated with"
                        (code malloc))))
       (list (make-gc-picture/ann
              `(rc moved) `() null #f
              (ann-item "Adjust counts when a pointer is changed...")))
       (list (make-gc-picture/ann
              `(rc moved freed) `() null #f
              (ann-item "... freeing an object if its count goes to 0")))
       (list (make-gc-picture/ann
              `(rc moved freed gone) `() null #f
              (ann-item "Same if the pointer is in a register")))
       (list (make-gc-picture/ann
              `(rc moved freed gone dfreed) `() null #f
              (ann-item "Adjust counts after frees, too...")))
       (list (make-gc-picture/ann
              `(rc moved freed gone dfreed ffreed) `() null #f
              (ann-item "... which can trigger more frees")))))

(slide
 #:title "Reference Counting in an Interpreter"
 #:layout 'tall
 (sscode
  ...
  [(lamE body-expr)
   (begin
     (ref- v-reg)
     (set! v-reg
           (code:comment "must ref+ env:")
           (closV body-expr env-reg))
     (ref+ v-reg)
     (continue))]
  ...
  [(doAppK fun-val k)
   (begin
     (set! expr-reg (closV-body fun-val)) (code:comment "code is static")
     (ref- env-reg)
     (set! env-reg
           (code:comment "must ref+ each arg:")
           (cons v-reg (closV-env fun-val)))
     (ref+ env-reg) (code:comment "=> ref+ on v-reg")
     (ref+ k)
     (ref- k-reg) (code:comment "=> ref- on fun-val and k")
     (set! k-reg k)
     (interp))]))

(slide
 #:title "Reference Counting And Cycles"
 'alts
 (list (list (make-gc-picture/ann
              `(rc moved freed) `() null #f
              (ann-item "An assignment can create a cycle...")))
       (list (make-gc-picture/ann
              `(rc moved freed cycle) `() null #f
              (ann-item "Adding a reference increments a count")))
       (list (make-gc-picture/ann
              `(rc moved freed cycle gone) `() null #f
              (ann-item "Lower-left objects are inaccessible, but not deallocated")
              (ann-item "In general, cycles break reference counting")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 3)

(slide
 #:title "Garbage Collection"
 (para (bit "Garbage collection:")
       "a way to know whether an object is"
       (it "accessible"))
 'next
 (blank)
 (item "An object referenced by a register is" (bit "live"))
 (item "An object referenced by a live object is also live")
 (item "A program can only possibly use live objects, because"
       "there is no way to get to other objects")
 'next
 (blank)
 (item "A garbage collector frees all objects that are not live")
 (item "Allocate until we run out of memory, then"
       "run a garbage collector to get more space"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title "Garbage Collection Algorithm"
 (item "Color all objects" (bit "white"))
 (item "Color objects referenced by registers" (bit "gray"))
 (item "Repeat until there are no gray objects:")
 (subitem "Pick a gray object," (it "r"))
 (subitem "For each white object that" (it "r") "points to,"
          "make it gray")
 (subitem "Color" (it "r") (bit "black"))
 (item "Deallocate all white objects"))


(slide
 #:title "Garbage Collection"
 'alts
 (list (list (make-gc-picture/ann
              `(gone cycle moved) `() `() #f
              (ann-item "All objects are marked white")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `(b) `() #f
              (ann-item "Mark objects referenced by registers as gray")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `(b) `() 'b
              (ann-item "Need to pick a gray object")
              (ann-item "Red arrow indicates the chosen object")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `(b a e) `() 'b
              (ann-item "Mark white objects referenced by chosen object as gray")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `(a e) `(b) 'b
              (ann-item "Mark chosen object black")))
       
       (list (make-gc-picture/ann
              `(gone cycle moved) `(a e) `(b) 'a
              (ann-item "Start again: pick a gray object")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `(e) `(b a) #f
              (ann-item "No referenced objects; mark black")))
       
       (list (make-gc-picture/ann
              `(gone cycle moved) `(e) `(b a) 'e
              (ann-item "Start again: pick a gray object")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `(e g) `(b a) 'e
              (ann-item "Mark white objects referenced by chosen object as gray")))
       (list (make-gc-picture/ann 
              `(gone cycle moved) `(g) `(b a e) #f
              (ann-item "Mark chosen object black")))     
       
       (list (make-gc-picture/ann
              `(gone cycle moved) `(g) `(b a e) 'g
              (ann-item "Start again: pick a gray object")))
       (list (make-gc-picture/ann
              `(gone cycle moved) `() `(b a e g) #f
              (ann-item "No referenced white objects; mark black")))                
       
       (list (make-gc-picture/ann
              `(gone cycle moved cyclegone freed) `() `(b a e g) #f
              (ann-item "No more gray objects; deallocate white objects")
              (ann-item "Cycles" 
                        (bit "do not")
                        "break garbage collection")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 4)

(slide
 #:title "Two-Space Copying Collectors"
 (para "A" (dt "two-space") "copying collector compacts"
       "memory as it collects, making allocation easier.")
 (para (colorize (bt "Allocator:") "blue"))
 (item "Partitions memory into" (bit "to-space")
       "and" (bit "from-space"))
 (item "Allocates only in" (bit "to-space"))
 (para (colorize (bt "Collector:") "blue"))
 (item "Starts by swapping"  (bit "to-space")
       "and" (bit "from-space"))
 (item "Coloring gray" sym:implies "copy from" 
       (bit "from-space") "to" (bit "to-space"))
 (item "Choosing a gray object" sym:implies "walk once though the new" 
       (bit "to-space") ", update pointers"))

(define (make-gc-picture/ann2 a b c d ann)
  (rt-superimpose
   (cc-superimpose
    (make-gc-picture a b c d)
    titleless-page)
   ann))

(define (ann-item2 . l)
  (apply para #:width ann-w l))

(slide
 #:title "Two-Space Collection"
 'alts
 (list (list (make-gc-picture/ann2
              `(gone cycle moved 2) `() `() #f
              (vc-append
               (current-line-sep)
               (ann-item2 "Left = from-space")
               (ann-item2 "Right = to-space"))))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy) `() `() #f
              (ann-item2 "Mark gray = copy and leave forward address")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy) `() `() 'b2
              (ann-item2 "Choose gray by walking through to-space")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy) `() `() 'b2
              (ann-item2 "Mark referenced as gray")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy) `() `() 'a2
              (ann-item2 "Mark black = move gray-choosing arrow")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy) `() `() 'e2
              (ann-item2 "Nothing to color gray; increment the arrow")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy gcopy) `() `() 'e2
              (ann-item2 "Color referenced object gray")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy gcopy) `() `() 'g2
              (ann-item2 "Increment the gray-choosing arrow")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy gcopy gupdate) `() `() 'g2
              (ann-item2 "Referenced is already copied, use forwarding address")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy gcopy gupdate) `() `() #f
              (ann-item2 "Choosing arrow reaches the end of to-space: done")))
       (list (make-gc-picture/ann2
              `(gone cycle moved 2 bcopy aecopy gcopy gupdate 2only) `() `() #f
              (vc-append
               (current-line-sep)
               (ann-item2 "Right = from-space")
               (ann-item2 "Left = to-space"))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(part 5)

(slide
 #:title "Two-Space Collection on Vectors"
 (item "Everything is a number:")
 (subitem "Some numbers are immediate integers")
 (subitem "Some numbers are pointers")
 (item "An allocated object in memory starts with a tag,"
       "followed by a sequence of pointers and immediate integers")
 (subitem "The tag describes the shape"))

(define addr-col "blue")

(define (mk-regs n a b)
  (para #:fill? #f
        (t (format "~aRegister 1:" n)) a
        (blank (* 3 (current-font-size)) 0) 
        (t (format "~aRegister 2:" n)) b))

(define (mk-ex num? start? to-phase)
  (define (rt s) (colorize (tt s) "red"))
  (slide
   #:title "Two-Space Vector Example"
   #:layout 'top
   (vl-append
    (* 4 (current-line-sep))
    (item "26-byte memory (13 bytes for each space), 2 registers")
    (subitem "Tag 1: one integer")
    (subitem "Tag 2: one pointer")
    (subitem "Tag 3: one integer, then one pointer"))
   (blank)
   (mk-regs "" 
            (if (to-phase . < . 2) (tt "7") (prog "0" "0"))
            (if (to-phase . < . 3) (tt "0") (prog "3" "3")))
   (table
    2
    (append
     (list (t "From:")                (prog "99 .." (format "~a ~a ~a ~a  3  2 10 ~a ~a  2  3  1  4"
                                                            (if (to-phase . > . 2) "99" " 1")
                                                            (if (to-phase . > . 2) " 3" "75")
                                                            (if (to-phase . > . 3) "99" " 2")
                                                            (if (to-phase . > . 3) " 5" " 0")
                                                            (if (to-phase . > . 1) "99" " 3")
                                                            (if (to-phase . > . 1) " 0" " 2"))))
     (if num?
         (list (colorize (t "Addr:") addr-col) (colorize (tt "00 01 02 03 04 05 06 07 08 09 10 11 12") addr-col))
         null)
     (if start?
         (list (blank)                        (colorize (tt " ^     ^     ^        ^        ^      ") addr-col))
         null)
     (if (zero? to-phase)
         null
         (cons (t "To:") 
               (case to-phase
                 [(1) (list (tt " 0  0  0  0  0  0  0  0  0  0  0  0  0") (blank)
                            (rt " ^") (blank)
                            (tt " ^"))]
                 [(2) (list (tt " 3  2  2  0  0  0  0  0  0  0  0  0  0") (blank)
                            (rt " ^") (blank)
                            (tt "          ^"))]
                 [(3) (list (tt " 3  2  2  1 75  0  0  0  0  0  0  0  0") (blank)
                            (rt " ^") (blank)
                            (tt "                ^"))]
                 [(4) (list (tt " 3  2  5  1 75  2  0  0  0  0  0  0  0") (blank)
                            (rt "          ^") (blank)
                            (tt "                      ^"))]
                 [(5) (list (tt " 3  2  5  1 75  2  0  0  0  0  0  0  0") (blank)
                            (rt "                ^") (blank)
                            (tt "                      ^"))]
                 [(6) (list (tt " 3  2  5  1 75  2  3  0  0  0  0  0  0") (blank)
                            (rt "                      ^") (blank)
                            (tt "                      ^"))]))))
    lc-superimpose lc-superimpose (current-font-size) (current-line-sep))))

(mk-ex #f #f 0)
(mk-ex #t #f 0)
(mk-ex #t #t 0)
(mk-ex #t #t 1)
(mk-ex #t #t 2)
(mk-ex #t #t 3)
(mk-ex #t #t 4)
(mk-ex #t #t 5)
(mk-ex #t #t 6)
