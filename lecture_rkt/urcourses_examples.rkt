#lang flit
;; works on lists of positive numbers
;; Input a list of numbers, output a number
(define (listMax [nums : (Listof Number)])
  : Number
  ;; Check if the input list is empty or not
  (type-case (Listof Number) nums
    [(empty)
     ;; define 0 to be the maximum of empty list,
     ;; since it will get replaced
     ;; by any numbers actually in the list
       0]
    ;; If the list is non-empty, then the biggest element
    ;; is either in the head or the tail,
    ;; and if it's in the tail, then it must also be the biggest
    ;; element of the tail.
    ;; How do we find the biggest element of the tail? recursion!
    [(cons num1 otherNums)
      (let* ([tailMax (listMax otherNums)])
        ;; If the head is bigger than the biggest tail element, then it must be
        ;; the biggest in the whole list.
        ;; Otherwise, the tail max is the max of the whole list
        (if (>= num1 tailMax)
            num1
            tailMax))]) )

(listMax '(1 2 4 99 3 2 5 7))

;; Add an element to the end of a list
(define (snoc [someList : (Listof 'a)] [x : 'a] )
  : (Listof 'a)
  (type-case (Listof 'a) someList
       ;; To add an element to the end of an empty list,
       ;; we get a list with one element
    [(empty)
       (list x)]
    ;; To add an element to the end of a list with a head,
    ;; we can add it to the end of the tail (with recursion), and then attach the head
    ;; to the new list.
    ;; We're allowed to add it to the tail, because we can always call ourselves recursively
    ;; on the tail of the list we're working on.
    [(cons h t)
       (cons h (snoc t x))]
    ))
(snoc (list 2 4 5 9 7 1) 3)