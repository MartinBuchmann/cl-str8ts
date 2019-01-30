;; Time-stamp: <2019-01-30 21:46:31 Martin>
;; * str8ts.lisp
;; See Readme.org for more information
;;
;; * The package
(in-package #:str8ts)

;; Testing...
(prove:plan 3)

;; * Defining the data structures

(deftype value ()
  "A new type for the possible values."
  '(integer -9 10))

(deftype candidates ()
  "A new type for possible values of each puzzle field."
  '(simple-array (bit) 9))

;; ** Testing the new types
(prove:ok (typep 4 'value))
(prove:ok (not (typep -10 'value)))

;; TODO: Think about an nine digit bit-vector indicating the possible candidates
;;
;; I don't know what is the best data structure yet but will start with an
;; 2D-array holding the lines and columns.
(defvar *the-puzzle* (make-array (list 9 9) :element-type 'value))

;; * Read in a puzzle
(defun read-puzzle (&optional (file #p"medium.txt"))
  "Read in the puzzle's data where each puzzle consists of one line:

A 10 indicates a black field, negative numbers are numbers within black fields,
the given numbers and the empty fields are represented by 0.
"
  (iter (for v in-file file)
        (for i from 0)
        (setf (row-major-aref *the-puzzle* i) v)))

(prove:is 81 (length (read-puzzle)))

;; TODO: Implement a simple print-puzzle function

;; * The predicate(s)

;; * Solving the puzzle

;; * Testing

(prove:finalize)
