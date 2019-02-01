;; Time-stamp: <2019-02-01 18:09:03 Martin>
;; * str8ts.lisp
;; See Readme.org for more information
;;
;; * The package
(in-package #:str8ts)

;; Testing...
(prove:plan 6)

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
(defun read-puzzle (&optional (file #p"puzzles/2019-01-26-medium")
                    &aux (puzzle (make-array (list 9 9) :element-type 'value)))
  "Read in the puzzle's data where each puzzle consists of one line:

A 10 indicates a black field, negative numbers are numbers within black fields,
the given numbers and the empty fields are represented by 0.
"
  (iter (for v in-file file)
        (for i from 0)
        (setf (row-major-aref puzzle i) v))
  puzzle)

;; ** Testing the data input
(prove:is 81 (array-total-size (read-puzzle)))
(prove:is 81 (array-total-size (read-puzzle #p"puzzles/2019-01-26-solved")))

;; * Printing the puzzle
(defun print-puzzle (puzzle i)
  "Prints the current PUZZLE depending on step I."
  (let ((size (array-dimension puzzle 0)))
    (cond ((equal i 0) (format t "~&Initial puzzle:~%"))
	  ((numberp i) (format t "~%~% Round: ~D~%" i))
	  ((equal i 'Rätsel) (format t "~%New puzzle:~%"))
	  (t (format t "~&Final state:~%")))
    (format t " ~v@{~A~:*~}~%" (1- (* 6 size)) "-")
    (iter 
      (for i below size)
      (iter 
        (for j below size)
        (format t "| ~3D " (aref puzzle i j)))
      (format t "|~%"))
    (format t " ~v@{~A~:*~}" (1- (* 6 size)) "-"))
  i)  ; Returns the current step
 
;; ** Testing the printing
(prove:is 0 (print-puzzle (read-puzzle) 0))
(prove:is t (print-puzzle (read-puzzle #p"puzzles/2019-01-26-solved") t))

;; * The predicate(s)
;;
;; In a first step I just check for empty fields.  Maybe I have to re-fine this
;; later...

;; Using this predicate I have to make sure that the logic of filling the puzzle
;; is correct.
(defun solvedp (puzzle)
  "Returns T if PUZZLE is completely solved."
  (iter (for i below (array-total-size puzzle))
        (never (zerop (row-major-aref puzzle i)))))

;; ** Testing the predicate
(prove:ok (solvedp (read-puzzle #p"puzzles/2019-01-26-solved")))
(prove:ok (not (solvedp (read-puzzle))))

;; * Solving the puzzle

;; * Testing

(prove:finalize)
