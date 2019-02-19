;; Time-stamp: <2019-02-19 21:37:45 Martin>
;; * str8ts.lisp
;;
;; Copyright (C) 2019 Martin Buchmann
;;
;; Author: Martin Buchmann <Martin.Buchmann@gmail.com>
;; GIT: https://github.com/MartinBuchmann/cl-str8ts
;; Version: 0.1
;; Created: 2019-02-10
;; Keywords: common-lisp str8ts solver
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; See Readme.org for more information
;;
;; * The package
(in-package #:str8ts)

;; Testing using prove
(prove:plan 13)

;; * Defining the data structures
;;
;; A puzzle consists of 81 fields (9x9) where each field can take a value from 1
;; to 9. Blocked fields are encoded using =10= and blocked values negative
;; integers.

;; ** A new type for the legal values
;; of a field.
(deftype value ()
  "A new type for the possible values."
  '(integer -9 10))

;; ** A new class for the puzzle
;; I am using a new class for the puzzle holding the current puzzle grid to
;; solve and the possible values.  To save space the possible values are stored
;; using the bits of an nine bit integer.
(defclass puzzle ()
  ((grid :documentation "The current sudoku grid."
	 :initform (make-array '(9 9)
			       :element-type 'value
			       :initial-element 0)
	 :initarg :grid)
   (values :documentation "All possible values for each field, as bits."
	   :initform (make-array '(9 9)
				 :element-type '(integer 0 #b111111111)
				 :initial-element #b111111111)
	   :initarg :values)))

;; ** Copying the puzzle
(defgeneric copy-puzzle (puzzle))
(defun copy-puzzle-array (array
                          &aux (copy (make-array '(9 9) :element-type (array-element-type array))))
  "Copy the given grid."
  (iter (for i below 81)
        (setf (row-major-aref copy i)
              (row-major-aref array i)))
  copy)

(defmethod copy-puzzle ((puzzle puzzle))
  "Copy given PUZZLE into a whole new puzzle and return it"
  (with-slots (grid values) puzzle
   (make-instance 'puzzle
		  :grid (copy-puzzle-array grid)
		  :values (copy-puzzle-array values))))

;; ** Testing the new types
(prove:ok (typep 4 'value))
(prove:ok (not (typep -10 'value)))
(prove:is (slot-value (make-instance 'puzzle) 'grid)
          '#2A((0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0 0)) :test #'equalp)

;; ** Handling the bit-vector representation
;;
;; Taken from:
;; https://github.com/dimitri/sudoku/blob/master/sudoku.lisp
;;
(defun count-remaining-possible-values (possible-values)
  "How many possible values are left in there?"
  (logcount possible-values))

(defun first-set-value (possible-values)
  "Return the index of the first set value in POSSIBLE-VALUES."
  (+ 1 (floor (log possible-values 2))))

(defun only-possible-value-is? (possible-values value)
  "Return a generalized boolean which is true when the only value found in
   POSSIBLE-VALUES is VALUE"
  (and (logbitp (- value 1) possible-values)
       (= 1 (logcount possible-values))))

(defun list-all-possible-values (possible-values)
  "Return a list of all possible values to explore"
  (loop for i from 1 to 9
     when (logbitp (- i 1) possible-values)
     collect i))

(defun value-is-set? (possible-values value)
  "Return a generalized boolean which is true when given VALUE is possible
   in POSSIBLE-VALUES"
  (logbitp (- value 1) possible-values))

(defun unset-possible-value (possible-values value)
  "return an integer representing POSSIBLE-VALUES with VALUE unset"
  (logxor possible-values (ash 1 (- value 1))))

;; * Read in a puzzle

(defmethod read-grid ((puzzle puzzle) &optional (file #p"puzzles/2019-01-26-medium"))
  "Parse a given GRID-STRING and fill PUZZLE, accordingly."
  (with-slots (grid values) puzzle
    (iter (for v in-file file)
          (for i from 0)
          (setf (row-major-aref grid i) v))
    puzzle))

;; ** Create a puzzle object.
(defun make-puzzle (&optional (file #p"puzzles/2019-01-26-medium"))
  "Make a str8ts puzzle from the given FILE.

A 10 indicates a black field, negative numbers are numbers within black fields,
the given numbers and the empty fields are represented by 0."
  (let ((p (make-instance 'puzzle)))
    (read-grid p file)))

;; *** Testing the data input
(prove:is 81 (array-total-size (slot-value (make-puzzle) 'grid)))
(prove:is 81 (array-total-size (slot-value (make-puzzle #p"puzzles/2019-01-26-solved") 'grid)))

;; ** Generate the units of the puzzle
;;
;; Each puzzle has different units due to the blocked fields and str8ts puzzle
;; have no boxes but sub-units.  First we will find all units (the column and
;; the row) of the given field (row . col).
(defmethod list-units-containing ((puzzle puzzle) row col)
  "List the indexes of all units of a given position."
  (with-slots (grid) puzzle
    (values
     ;; unit with peers on the same row
     (iter
       (for pc below 9)
       (unless (or (= pc col)                ; The field itself
                   (= 10 (aref grid row pc)) ; Blocked fields
                   (minusp (aref grid row pc)))
         (collect (cons row pc))))

     ;; unit with peers on the same column
     (iter
       (for pr below 9)
       (unless (or (= pr row)                ; The field itself
                   (= 10 (aref grid pr col)) ; Blocked fields
                   (minusp (aref grid pr col)))
         (collect (cons pr col)))))))

;; Then we will split the units in the sub-units.
(defmethod find-sub-units ((puzzle puzzle) row col)
  "Returns lists of the sub-units for ROW and COL."
  (with-slots (grid) puzzle
    (multiple-value-bind (r c)
        (list-units-containing puzzle row col)
      (values (split-sub-rows r)
              (split-sub-cols c)))))

(defun split-sub-rows (row)
  "Returns the sub-units of ROW."
  (flet ((split-aux (l)
           (iter (for (x . y) on l)
                 (collect x into a)
                 (when (and y (/= (cdar y) (1+ (cdr x))))
                   (return (list a y)))      ; split here
                 (finally (return (list a nil)))))) ; never splits
    (iter (for (a b) first (split-aux row) then (split-aux b))
          (collect a)
          (while b))))  ; repeat splitting over them

(defun split-sub-cols (col)
  "Returns the sub-units of COL."
  (flet ((split-aux (l)
           (iter (for (x . y) on l)
                 (collect x into a)
                 (when (and y (/= (caar y) (1+ (car x))))
                   (return (list a y)))      ; split here
                 (finally (return (list a nil)))))) ; never splits
    (iter (for (a b) first (split-aux col) then (split-aux b))
          (collect a)
          (while b))))  ; repeat splitting over them

;; *** Testing the sub-unit splitting
(prove:is (split-sub-rows '((0 . 0) (0 . 3))) '(((0 . 0)) ((0 . 3))))
(prove:is (split-sub-cols '((0 . 0) (3 . 0))) '(((0 . 0)) ((3 . 0))))

;; ** Generate the units and sub-units of the whole puzzle
(defmethod puzzle-units ((puzzle puzzle)
                         &aux (units
                               (make-array '(9 9) :element-type 'list :initial-element nil)))
  "Returns an array with all units of PUZZLE and NIL for the blocked fields."
  (with-slots (grid) puzzle
    (iter
     (for row below 9)
     (iter
       (for col below 9)
       (unless (or (= 10 #1=(aref grid row col)) (minusp #1#))
         (log:info "R: ~D C: ~D" row col)
         (setf (aref units row col)
	       (multiple-value-bind (peers-row peers-col)
		   (list-units-containing puzzle row col)
	         (list peers-row peers-col)))))))
  units)

;; *** Is the given field within the current sub-unit?
(defun in-sub-unit-p (row col sub-unit)
  "Returns the sub-unit if (row . col) is part of SUB-UNIT."
  ;; Out of bound indexes do not matter here and save further checking.
  (let ((candidates (list (cons (1- row) col) (cons (1+ row) col)
                          (cons row (1- col)) (cons row (1+ col)))))
    (some (lambda (u) (member u sub-unit :test #'equalp)) candidates)))


(defmethod puzzle-sub-units ((puzzle puzzle)
                             &aux (sub-units
                                   (make-array '(9 9) :element-type 'list :initial-element nil)))
  "Returns an array with all sub-units of PUZZLE and NIL for the blocked fields."
  (with-slots (grid) puzzle
    (iter
     (for row below 9)
     (iter
       (for col below 9)
       (unless (or (= 10 #2= (aref grid row col)) (minusp #2#))
         (setf (aref sub-units row col)
               (multiple-value-bind (su-row su-col)
                   (find-sub-units puzzle row col)
                 (remove-if-not (curry #'in-sub-unit-p row col)
                                (append su-row su-col))))))))
  sub-units)

;; * Printing the puzzle
(defmethod print-puzzle ((puzzle puzzle) i)
  "Prints the current GRID depending on step I."
  (with-slots (grid) puzzle
    (let ((size (array-dimension grid 0)))
    (cond ((equal i 0) (format t "~&Initial puzzle:~%"))
	  ((numberp i) (format t "~%~% Round: ~D~%" i))
	  ((equal i 'Rätsel) (format t "~%New puzzle:~%"))
	  (t (format t "~&Final state:~%")))
    (format t " ~v@{~A~:*~}~%" (1- (* 6 size)) "-")
    (iter 
      (for i below size)
      (iter 
        (for j below size)
        (format t "| ~3D " (aref grid i j)))
      (format t "|~%"))
    (format t " ~v@{~A~:*~}" (1- (* 6 size)) "-"))
    i))   ; Returns the current step
 
;; ** Testing the printing
(prove:is 0 (print-puzzle (slot-value (make-puzzle) 'grid) 0))
(prove:is t (print-puzzle (slot-value (make-puzzle #p"puzzles/2019-01-26-solved") 'grid) t))

;; * The predicates
;;
;; ** Is the puzzle solved?
;; In a first step I just check for empty fields.  Maybe I have to re-fine this
;; later...

;; Using this predicate I have to make sure that the logic of filling the puzzle
;; is correct.
(defmethod solvedp ((puzzle puzzle))
  "Returns T if PUZZLE is completely solved."
  (with-slots (grid) puzzle
    (iter (for i below (array-total-size grid))
      (never (zerop (row-major-aref grid i))))))

;; ** Is the sequence of fields valid?
;;
;; While the order may or is likely to be disturbed all digits within
;; a subunit must be consecutive.
(defun valid-subunit-p (sub-unit)
  "Returns true if SUB-UNIT is identical to min..max as a set."
  (flet ((sub-unit-aux (sub-unit)
           ;; Because I have to scan for the minimum I will determine
           ;; the length in the same task
           (iter
             (with min = 11) ; The maximum value should be 10 
             (for i from 0)
             (for obj in sub-unit)
             (when (< obj min)
               (setf min obj))
             (finally (return (values i min))))))
    (multiple-value-bind (len min)
        (sub-unit-aux sub-unit)
      (iter
        (with indicator = (make-array len :element-type 'bit :initial-element 0))
        (for obj in sub-unit)
        (unless (< (- obj min) len)
          ;; obj out of range
          (return-from valid-subunit-p nil))
        (if (zerop (aref indicator (- obj min)))
            ;; obj is being seen for the 1st time; record that
            (setf (aref indicator (- obj min)) 1)
            ;; duplicate obj
            (return-from valid-subunit-p nil)))
      ;; all list elements are unique and in the right range;
      ;; injectivity + same cardinality for finite sets => surjectivity
      t)))

;; ** Testing the predicates
(prove:ok (solvedp (make-puzzle #p"puzzles/2019-01-26-solved")))
(prove:ok (not (solvedp (make-puzzle))))
(prove:ok (valid-subunit-p '(2 4 3 1)))
(prove:ok (not (valid-subunit-p '(2 3 5 6))))

;; * Solving the puzzle
;; ** Constrained propagation
;;
;; TODO: Assign the values and eliminate the given and impossible

;; ** Depth-First search

;; * Testing

(prove:finalize)

