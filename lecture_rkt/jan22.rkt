#lang flit

(define (range [start : Number] [end : Number]
               [maybeGap : (Optionof Number)])
  : (Listof Number)
  ;; Default gap of 1 if nothing is given
  (let* ([gap (type-case (Optionof Number) maybeGap
                [(none) 1]
                [(some x) x])])
   (if (<= end start)
       ;; Base case, nothing in range if end is before start
      '()
      ;; Recursive case: include the start
      ;; then add that to the range starting at the next number
      (cons start (range (+ gap start) end maybeGap))
      )))


(define (lookup [key : Number]
                [pairs : (Listof (Number * 'data))])
  : (Optionof 'data)
  (type-case (Listof (Number * 'data)) pairs
    [empty ;; If list is empty, then the key can't be present
       (none)]
    [(cons h t)
       (if (= key (fst h)) ;; If the key matches, we're done
           (some (snd h))
           (lookup key t))]))


(define-type (Either 'a 'b)
  (Left [inLeft : 'a])
  (Right [inRight : 'b])
)


;; Have (Number * ')

(define (lookupUnique [key : Number]
           [pairs : (Listof (Number * 'data))])
  : (Either String 'data)
  (type-case (Listof (Number * 'data)) pairs
    [empty ;; If list is empty, then the key can't be present
       (Left "Key not found!")]
    [(cons headPair otherPairs)
       (let* ([rec (lookupUnique key otherPairs)]
              [foo TODO])
       (if (= key (fst TODO)) ;; Check for dupes
           (type-case (Either String 'data) rec
             ;; If head has key but tail doesn't, success
             [(Left msg) (Right (snd headPair))]
             [(Right val) (Left "Duplicate keys in list")])
           ;; If head didn't have key
           ;; Then success/failure is what tail found
           rec))]))

(define students
  (list (pair 0001 "Bob")
        (pair 0002 "Alice")
        (pair 0003 "Eve")
        (pair 0004 "Bob"))
  )

(define-type NonEmptyList
  (Last [x : Number])
  (NECons [head : Number] [tail : NonEmptyList]))

(define-type Nat
  (Z)
  (Successor (n : Nat)))

(define-type Filesystem
  (File [name : String]
        [data : Number])
  (Folder [name : String]
          [contents : (Listof Filesystem)]))
