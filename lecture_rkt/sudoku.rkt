#lang flit

;; A Sudoku is a list of numbers that may be blank
;; We'll make sure they have 81 elements,
;; but we can't encode that in types
(define-type-alias Sudoku (Listof (Optionof Number)))

(define solvedExample : Sudoku
  (map some
       (list 5 3 4 6 7 8 9 1 2
             6 7 2 1 9 5 3 4 8
             1 9 8 3 4 2 5 6 7
             8 5 9 7 6 1 4 2 3
             4 2 6 8 5 3 7 9 1
             7 1 3 9 2 4 8 5 6
             9 6 1 5 3 7 2 8 4
             2 8 7 4 1 9 6 3 5
             3 4 5 2 8 6 1 7 9
             )))

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
(define nums0-9 (list 0 1 2 3 4 5 6 7 8))

;; To get the nth row, we start at 9*row and then take the next 9 elements
(define (nthRow [sud : Sudoku] [rowIndex : Number])
  : GroupOfNine
  (map (lambda (colIndex) (list-ref sud (+ colIndex (* 9 rowIndex))))
       nums0-9))

;; Get the list of all rows in a sudoku
;; by getting the nth row for n in (list 0 ... 8)
(define (rowsOf [sud : Sudoku]) : (Listof GroupOfNine)
  (map (lambda (rowIndex) (nthRow sud rowIndex))
       nums0-9))

;; To get the nth column, we take the nth element of each row
(define (nthColumn [sud : Sudoku] [colIndex : Number])
  : GroupOfNine
  (let ([rows (rowsOf sud)])
    (map (lambda (row) (list-ref row colIndex))
         rows)))

;; The list of all columns of a sudoku
(define (columnsOf [sud : Sudoku]) : (Listof GroupOfNine)
  (map (lambda (colIndex) (nthColumn sud colIndex))
       nums0-9))

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

;;Next, we define when a sudoku is valid.
;; A sudoku is valid if there are no duplicates in any
;; row, column, or square,
;; and it has 81 elements.