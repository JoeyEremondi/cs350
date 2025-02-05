#lang flit

;; A Sudoku is a list of numbers that may be blank
;; We'll make sure they have 81 elements,
;; but we can't encode that in types
(define-type-alias Sudoku (Listof (Optionof Number)))

;; Helpful function to turn 0s into (none) and all others into (some x)
(define (readNum [x : Number]) : (Optionof Number)
  ;; Error if number not in range
  (if (or (< x 0) (> x 9))
      (error 'read "Not a valid Sudoku number")
      ;; Otherwise, turn 0 into (none) and all others into (some x)
      (if (zero? x)
          (none)
          (some x))))

(define (readSudoku [nums : (Listof Number)]) : Sudoku
  (map readNum nums))
 
(define unsolvedExample : Sudoku
  (readSudoku
   (list 5 3 0 0 7 0 0 0 0
         6 0 0 1 9 5 0 0 0
         0 9 8 0 0 0 0 6 0
         8 0 0 0 6 0 0 0 3
         4 0 0 8 0 3 0 0 1
         7 0 0 0 2 0 0 0 6
         0 6 0 0 0 0 2 8 0
         0 0 0 4 1 9 0 0 5
         0 0 0 0 8 0 0 7 9
         )))

;; For testing. There's two 8s
;; in the second last column
(define badExample : Sudoku
  (readSudoku
   (list 5 3 0 0 7 0 0 8 0
         6 0 0 1 9 5 0 0 0
         0 9 8 0 0 0 0 6 0
         8 0 0 0 6 0 0 0 3
         4 0 0 8 0 3 0 0 1
         7 0 0 0 2 0 0 0 6
         0 6 0 0 0 0 2 8 0
         0 0 0 4 1 9 0 0 5
         0 0 0 0 8 0 0 7 9
         )))

(define solvedExample : Sudoku
  (readSudoku
   (list 5 3 4 6 7 8 9 1 2
         6 7 2 1 9 5 3 4 8
         1 9 8 3 4 2 5 6 7
         8 5 9 7 6 1 4 2 3
         4 2 6 8 5 3 7 9 1
         7 1 3 9 2 4 8 5 6
         9 6 1 5 3 7 2 8 4
         2 8 7 4 1 9 6 3 5
         3 4 5 2 8 6 1 7 9)))

;; For testing. There's two 8s
;; in the second last column
(define oneMissing : Sudoku
  (readSudoku
   (list 5 3 4 6 7 8 9 1 2
         6 7 2 1 9 5 3 4 8
         1 9 8 3 4 2 5 6 7
         8 5 9 7 6 1 4 2 3
         4 2 6 8 5 3 7 9 1
         7 1 3 9 2 4 8 5 6
         9 6 1 5 3 7 2 8 4
         2 8 7 4 1 9 6 3 5
         3 4 5 2 8 6 1 7 0)))

(define hardSudoku
  (readSudoku
   (list 7 0 0 6 0 0 2 0 0
         0 0 2 0 0 9 0 0 0
         0 5 0 0 7 0 0 0 8
         6 0 0 4 0 0 0 1 0
         0 0 8 0 1 0 3 0 0
         0 9 0 0 0 2 0 0 5
         9 0 0 0 3 0 0 5 0
         0 0 0 7 0 0 1 0 0
         0 0 3 0 0 8 0 0 4)))

(define hardestSudoku
  (readSudoku
   (list 8 0 0 0 0 0 0 0 0
         0 0 3 6 0 0 0 0 0
         0 7 0 0 9 0 2 0 0
         0 5 0 0 0 7 0 0 0
         0 0 0 0 4 5 7 0 0
         0 0 0 1 0 0 0 3 0
         0 0 1 0 0 0 0 6 8
         0 0 8 5 0 0 0 1 0
         0 9 0 0 0 0 4 0 0)))

;; We'll index a sudoku like this
;; 0 2   ... 9
;; 9 11 ... 18
;; ...
;; 72 74 ... 80

;; e.g the first 9 elements are the first row, the next 9 are the second row, etc.

;; Useful helper function
;; TODO: move this to the stdlib
(define (concatAll [lists : (Listof (Listof 'a))])
  : (Listof 'a)
  (type-case (Listof (Listof 'a)) lists
    [(empty)
     (empty)]
    [(cons headList otherLists)
     (append headList (concatAll otherLists))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rows, columns, and squares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;We can get the rows, columns, and squares of a sudoku
;; as lists.
(define-type-alias GroupOfNine (Listof (Optionof Number)))

;;used a bunch
(define indices0-9 (list 0 1 2 3 4 5 6 7 8))

;; Like indices, but starting at 1
(define nums1-10 (list 1 2 3 4 5 6 7 8 9))

;; To get the nth row, we start at 9*row and then take the next 9 elements
(define (nthRow [sud : Sudoku] [rowIndex : Number])
  : GroupOfNine
  (map (lambda (colIndex) (list-ref sud (+ colIndex (* 9 rowIndex))))
       indices0-9))

;; Get the list of all rows in a sudoku
;; by getting the nth row for n in (list 0 ... 8)
(define (rowsOf [sud : Sudoku]) : (Listof GroupOfNine)
  (map (lambda (rowIndex) (nthRow sud rowIndex))
       indices0-9))

;; To get the nth column, we take the nth element of each row
(define (nthColumn [sud : Sudoku] [colIndex : Number])
  : GroupOfNine
  (let ([rows (rowsOf sud)])
    (map (lambda (row) (list-ref row colIndex))
         rows)))

;; The list of all columns of a sudoku
(define (columnsOf [sud : Sudoku]) : (Listof GroupOfNine)
  (map (lambda (colIndex) (nthColumn sud colIndex))
       indices0-9))

;; To get the square at position (i,j) on a 3x3 grid,
;; we take elements (3i, 3i+1, 3i+2) of rows (3j, 3j+1, 3j+2)
(define (square-ij [sud : Sudoku]
                   [i : Number]
                   [j : Number])
  : GroupOfNine
  (concatAll (map (lambda (rowIndex)
                    (map (lambda (colIndex) (list-ref (nthRow sud rowIndex) colIndex))
                         (map (lambda (x) (+ (* 3 j) x))
                              (list 0 1 2))))
                  (map (lambda (x) (+ (* 3 i) x))
                       (list 0 1 2)))))

(define (squaresOf [sud : Sudoku]) : (Listof GroupOfNine)
  (map (lambda (ij) (square-ij sud (pairFst ij) (pairSnd ij)))
       (concatAll
        (map (lambda (i)
               (map (lambda (j) (pair i j))
                    (list 0 1 2)))
             (list 0 1 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Valid solutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A row/column/square is possibly a valid solution
;; if it has no duplicates

;; Check if a predicate is true for all elements in a list
(define (all? [pred : ('a -> Boolean)]
              [elems : (Listof 'a)]) : Boolean
  (empty? (filter (lambda (x) (not (pred x))) elems)))

;; Check if a predicate is true for any element of a list
(define (any? [pred : ('a -> Boolean)]
              [elems : (Listof 'a)]) : Boolean
  (not (empty? (filter pred elems))))


;; Check if a list contains duplicates
(define (allUnique? [elems : (Listof 'a)])
  : Boolean
  ;; For each element, there should be exactly one value in elems that is equal to it
  ;; So we filter out all the elements equal to it, and check that the length is 1
  (all? (lambda (elem)
          (let ([equalElements
                 (filter (lambda (other) (equal? elem other))
                         elems)])
            (= (length equalElements) 1)))
        elems))

;; Check if the (some ...) elements of a list of Options
;; are all unique
(define (allPresentUnique? [elems : (Listof (Optionof 'a))])
  : Boolean
  (allUnique? (filter some? elems))
  )

;; A sudoku is valid if all of the numbers in the rows/columns/squares are unique.
;; We filter out any elements that aren't present,
;; so we can check if a partial solution is valid.

(define (validSudoku? [sud : Sudoku])
  : Boolean
  (and
   (all? allPresentUnique? (rowsOf sud))
   (all?  allPresentUnique? (columnsOf sud))
   (all? allPresentUnique? (squaresOf sud))))

;; A sudoku is complete if it doesn't have any missing numbers
(define (isComplete? [sud : Sudoku])
  (not (any? none? sud) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace the first (none) element of a list with (some x) for the given x.
;; Leave the list unchanged if there aren't any (none) in it
(define (replaceFirst [elems : (Listof (Optionof 'a))]
                      [newElement : 'a])
  : (Listof (Optionof 'a))
  (type-case (Listof (Optionof 'a)) elems
    [(empty)
     (empty)]
    [(cons h t)
     (if (none? h)
         ;; If the first element is (none), replace it with the new thing and stop looking
         (cons (some newElement) t)
         ;; Otherwise, attach the old first element to the result
         ;; of recursively replacing the first (none) in the rest of the list
         (cons h (replaceFirst t newElement)))]))

(define (findSolutions [sud : Sudoku])
  : (Listof Sudoku)
  ;; If the sudoku is already complete, then either it is solved,
  ;; or there is no solution
  (if (isComplete? sud)
      (if (validSudoku? sud)
          ;; if it's valid, then there's one solution, and we return it
          (list sud)
          ;; If it's not valid, then there is no solution, so we return empty list
          '())
      ;; If the sudoku isn't complete, we try replacing a blank with each number
      ;; from 1-9.
      ;; We filter out the bad ones, and then find the solutions for each possibility
      (let ([potentialSolutions (map (lambda (x) (replaceFirst sud x)) nums1-10)]
            [viableSolutions (filter validSudoku? potentialSolutions)])
        (concatAll (map findSolutions viableSolutions))
        )))

;; Nice wrapper to print the solution to a sudoku
(define (solve [sud : Sudoku])
  : (Listof (Listof Number))
  (type-case (Listof Sudoku) (findSolutions sud)
    [(empty)
     (error 'none "No solutions found!")]
    [(cons soln rest)
     (if (not (empty? rest))
         (error 'multi "Multiple solutions found")
         ;; Otherwise, get rid of all the (some) and break into rows
         ;;so it prints nicely
         (map (lambda (row) (map some-v row)) (rowsOf soln))
         )]))
