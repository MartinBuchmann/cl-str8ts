;; -*- ispell-local-dictionary: "en_GB" -*-
;; Time-stamp: <2019-02-23 16:26:25 Martin>
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
  ((grid :documentation "The current str8ts grid."
	 :initform (make-array '(9 9)
			       :element-type 'value
			       :initial-element 0)
	 :initarg :grid)
   (digits :documentation "All possible values for each field, as bits."
	   :initform (make-array '(9 9)
				 :element-type '(integer 0 #b111111111)
				 :initial-element #b111111111)
	   :initarg :digits)))

;; * Error conditions
;;
;; To track contradictions I have defined two conditions.
(define-condition empty-digits (condition) ())
(define-condition unit-contains-contradictory-solution (condition) ())

;; * The generic functions
;;
;; I define generic function even if the methods have only purpose.
(defgeneric copy-puzzle (puzzle)
  (:documentation "Copy a given PUZZLE into a whole new puzzle and returns it."))

(defgeneric read-grid (puzzle &optional (file))
  (:documentation "Parse a given GRID-STRING and fill PUZZLE, accordingly."))

(defgeneric list-units-containing (puzzle row col)
  (:documentation "List the indexes of all units for a given position."))

(defgeneric find-sub-units (puzzle row col)
  (:documentation "Returns lists of the sub-units for ROW and COL."))

(defgeneric puzzle-units (puzzle)
  (:documentation "Returns an array with all units of PUZZLE and NIL for the blocked fields."))

(defgeneric puzzle-sub-units (puzzle)
  (:documentation "Returns an array with all sub-units of PUZZLE and NIL for the blocked fields."))

(defgeneric print-puzzle (puzzle i)
  (:documentation "Prints the current GRID depending on step I."))

(defgeneric solvedp (puzzle)
  (:documentation "Returns T if PUZZLE is completely solved."))

(defgeneric eliminate-value-in-sub-units (puzzle row col value)
  (:documentation "Eliminate the given VALUE from all sub-units of (row . col)."))

(defgeneric list-places-with-single-unit-solution (puzzle row col value)
  (:documentation "Check given PUZZLE for VALUE having a single choice of placement within
   all peers units, and return the list of of such places as (ROW . COL)."))

(defgeneric eliminate (puzzle row col value)
    (:documentation "Eliminate the given VALUE from possible digits in cell (ROW
   . COL) of PUZZLE, and propagate when needed."))

(defgeneric assign (puzzle row col value)
  (:documentation "Assign given VALUE in field (ROW . COL) of PUZZLE, eliminating all other
   digits for the given field and propagating that elimination to the sub-units."))

;; ** Copying the puzzle
(defun copy-puzzle-array (array
                          &aux (copy (make-array '(9 9) :element-type (array-element-type array))))
  "Copy the given grid."
  (iter (for i below 81)
        (setf (row-major-aref copy i)
              (row-major-aref array i)))
  copy)

(defmethod copy-puzzle ((puzzle puzzle))
  (with-slots (grid digits) puzzle
   (make-instance 'puzzle
		  :grid (copy-puzzle-array grid)
		  :digits (copy-puzzle-array digits))))

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
(defun count-remaining-possible-digits (possible-digits)
  "How many possible digits are left in there?"
  (logcount possible-digits))

(defun first-set-value (possible-digits)
  "Return the index of the first set value in POSSIBLE-DIGITS."
  (+ 1 (floor (log possible-digits 2))))

(defun only-possible-value-is? (possible-digits value)
  "Return a generalized boolean which is true when the only value found in
   POSSIBLE-DIGITS is VALUE"
  (and (logbitp (- value 1) possible-digits)
       (= 1 (logcount possible-digits))))

(defun list-all-possible-digits (possible-digits)
  "Return a list of all possible digits to explore"
  (loop for i from 1 to 9
     when (logbitp (- i 1) possible-digits)
     collect i))

(defun value-is-set? (possible-digits value)
  "Return a generalized boolean which is true when given VALUE is possible
   in POSSIBLE-DIGITS"
  (logbitp (- value 1) possible-digits))

(defun unset-possible-value (possible-digits value)
  "return an integer representing POSSIBLE-DIGITS with VALUE unset"
  (logxor possible-digits (ash 1 (- value 1))))

;; * Read in a puzzle

(defmethod read-grid ((puzzle puzzle) &optional (file #p"puzzles/2019-01-26-medium"))
  (with-slots (grid digits) puzzle
    ;; Filling the grid
    (iter
      (for v in-file file)
      (for i from 0)
      (setf (row-major-aref grid i) v))
    ;; Assigning the digits
    (iter
      (for f below 81)
      (for v = (row-major-aref grid f))
      (for (values r c) = (floor f 9))
      (when (<= -9 v 9)
        (assign puzzle r c v)))
    puzzle))

;; ** Generate the units of the puzzle
;;
;; Each puzzle has different units due to the blocked fields and str8ts puzzle
;; have no boxes but sub-units.  First we will find all units (the column and
;; the row) of the given field (row . col).
(defmethod list-units-containing ((puzzle puzzle) row col)
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

(defmethod puzzle-units ((puzzle puzzle))
  (with-slots (grid) puzzle
    (iter
      (with units = (make-array '(9 9) :element-type 'list :initial-element nil))
      (for row below 9)
      (iter
        (for col below 9)
        (unless (or (= 10 #1=(aref grid row col)) (minusp #1#))
          (setf (aref units row col)
	        (multiple-value-bind (peers-row peers-col)
		    (list-units-containing puzzle row col)
	          (list peers-row peers-col)))))
      (finally (return units)))))

(defun in-sub-unit-p (row col sub-unit)
  "Returns the sub-unit if (row . col) is part of SUB-UNIT."
  ;; Out of bound indexes do not matter here and save further checking.
  (let ((candidates (list (cons (1- row) col) (cons (1+ row) col)
                          (cons row (1- col)) (cons row (1+ col)))))
    (some (lambda (u) (member u sub-unit :test #'equalp)) candidates)))


(defmethod puzzle-sub-units ((puzzle puzzle))
  (with-slots (grid) puzzle
    (iter
      (with sub-units = (make-array '(9 9) :element-type 'list :initial-element nil))
      (for row below 9)
      (iter
        (for col below 9)
        (unless (or (= 10 #2=(aref grid row col)) (minusp #2#))
          (setf (aref sub-units row col)
                (multiple-value-bind (su-row su-col)
                    (find-sub-units puzzle row col)
                  (remove-if-not (curry #'in-sub-unit-p row col)
                                 (append su-row su-col))))))
      (finally (return sub-units)))))

;; *** The units and sub-units of the current puzzle
(defvar *units* (make-array '(9 9) :element-type 'list :initial-element nil)
  "The list of all possible units in a str8ts grid.")

(defvar *sub-units* (make-array '(9 9) :element-type 'list :initial-element nil)
  "list of all possible sub-units in a str8ts grid.")

;; ** Create a puzzle object.
(defun make-puzzle (&optional (file #p"puzzles/2019-01-26-medium"))
  "Make a str8ts puzzle from the given FILE.

A 10 indicates a black field, negative numbers are numbers within black fields,
the given numbers and the empty fields are represented by 0."
  (let ((p (make-instance 'puzzle)))
    (read-grid p file)
    (setq *units* (puzzle-units p)
          *sub-units* (puzzle-sub-units p))
    p))

;; *** Testing the data input
(prove:is 81 (array-total-size (slot-value (make-puzzle) 'grid)))
(prove:is 81 (array-total-size (slot-value (make-puzzle #p"puzzles/2019-01-26-solved") 'grid)))

;; * Printing the puzzle
(defmethod print-puzzle ((puzzle puzzle) i)
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
(prove:is 0 (print-puzzle (make-puzzle) 0))
(prove:is t (print-puzzle (make-puzzle #p"puzzles/2019-01-26-solved") t))

;; * The predicates
;;
;; ** Is the puzzle solved?
;; In a first step I just check for empty fields.  Maybe I have to re-fine this
;; later...

;; Using this predicate I have to make sure that the logic of filling the puzzle
;; is correct.
(defmethod solvedp ((puzzle puzzle))
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
           ;; Because I have to scan for the minimum I will determine the length
           ;; in the same task
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
;; *** Eliminating the impossible digits
(defmethod eliminate-value-in-sub-units ((puzzle puzzle) row col value)
  (iter
    (for su in (aref *sub-units* row col))
    (for (r . c) in su)
    (eliminate puzzle r c value)))

(defmethod list-places-with-single-unit-solution ((puzzle puzzle) row col value)
  (with-slots (digits) puzzle
    (iter
      (for unit in (aref *units* row col))
      (nconcing
       (destructuring-bind (n positions)
	   (iter
             (for (r . c) in unit)
	     (when (only-possible-value-is? (aref digits r c) value)
	       (count t into n)
               (collect (cons r c) into p))
	     (finally (return (list n p))))
	 ;; Simply ignore n = 0 and solve it later
	 (when (< 1 n)
	   ;; If more than one place in that unit accepts only the
	   ;; given value, that's a contradiction
	   (error 'unit-contains-contradictory-solution))
	 (when (= 1 n)
	   (list (first positions))))))))

(defmethod eliminate ((puzzle puzzle) row col value)
  (with-slots (grid digits) puzzle
    ;; If already unset, work is already done
    (when (value-is-set? (aref digits row col) value)
      ;; Eliminate the value from the set of possible digits
      (let* ((possible-digits
	       (unset-possible-value (aref digits row col) value)))
	(setf (aref digits row col) possible-digits)

	;; Now if we're left with a single possible value
	(when (= 1 (count-remaining-possible-digits possible-digits))
	  (let ((found-value (first-set-value possible-digits)))
	    ;; Update the main grid
	    (setf (aref grid row col) found-value)

	    ;; Eliminate that value we just found in all peers
	    (eliminate-value-in-sub-units puzzle row col found-value)))
        ;; Now check if any unit has a single possible place for that value
	(iter (for (r . c) in (list-places-with-single-unit-solution puzzle row col value))
              (assign puzzle r c value))))))

;; *** Assigning a value
(defmethod assign ((puzzle puzzle) row col value)
  (with-slots (grid digits) puzzle
    (setf (aref grid row col) value)	; Assign the value to the grid 
    (iter
      (for other-value from 1 to 9)	; Eliminate the others digits
      (unless (= other-value value)
        ;; TODO: Fixing the bug in eliminate
        ;; (eliminate puzzle row col other-value)
        )))
  puzzle)

;; ** Depth-First search

;; * Testing

(prove:finalize)

