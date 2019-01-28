;; Time-stamp: <2019-01-28 20:38:40 Martin>
;; * str8ts.lisp
;; See Readme.org for more information
;;
;; * The package
(in-package #:str8ts)

;; Testing...
(prove:plan 2)

;; * Defining the data structures

(deftype candidate ()
  "A new type of the possible values of a puzzle field."
  '(integer -9 9))

;; ** Testing the new types
(prove:ok (typep 4 'candidate))
(prove:ok (not (typep -10 'candidate)))

;; TODO: Think about an nine digit bit-vector indicating the possible candidates
;;
;; I don't know what is the best data structure yet but will start with an
;; 2D-array holding the lines and columns.
(defvar *the-puzzle* (make-array (list 9 9) :element-type 'candidate))

;; * Read in a puzzle
(defun read-puzzle (&optional (file #p"medium.txt"))
  "Read in the puzzle's data where each puzzle consists of one line:

A zero indicates a black field, negative numbers are numbers within black fields,
the given numbers and the empty fields are represented by \".\"
"
  (with-open-file (puzzle file)
    (read-line puzzle)))

;; TODO: Implement a simple print-puzzle function

;; * The predicate(s)

;; * Solving the puzzle

;; * Testing

(prove:finalize)
