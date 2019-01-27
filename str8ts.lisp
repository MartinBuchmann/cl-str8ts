;; Time-stamp: <2019-01-27 23:03:21 Martin>
;; * str8ts.lisp
;; See Readme.org for more information
;;
;; * The package
(in-package #:str8ts)

;; Testing...
(prove:plan 2)

;; * Defining the data structures
;;
;; I don't know what is the best data structure yet but will start with an
;; 3D-array holding the lines, columns and in the third dimension the fixed
;; values.
(defvar *the-puzzle* (make-array (list 9 9 2)))
;; TODO: Decide which element type fits best.

;; * Read in a puzzle
(defun read-puzzle (&optional (file #p"medium.txt"))
  "Read in the puzzle's data where each puzzle consists of two lines:

- The first line indicates the black fields (X), the given numbers and the empty fields (.) 
- The second line consists of the blocked numbers in the black fiels and open fields (.)
"
  (with-open-file (puzzle file)
    (read-line puzzle)))

;; TODO: Read in both lines
;; TODO: Implement a simple print-puzzlr function

;; * The predicate(s)

;; * Solving the puzzle

;; * Testing

(prove:finalize)
