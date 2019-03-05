;; -*- ispell-local-dictionary: "en_GB" -*-
;; Time-stamp: <2019-03-05 21:02:39 Martin>
;; * str8ts.lisp
;;
;; Copyright (C) 2019 Martin Buchmann
;;
;; Author: Martin Buchmann <Martin.Buchmann@gmail.com>
;; GIT: https://github.com/MartinBuchmann/cl-str8ts
;; Version: 1.1
;; Created: 2019-02-10
;; Keywords: common-lisp str8ts solver
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; See Readme.org for more information
;;
;; * The package
(in-package #:str8ts)
(annot:enable-annot-syntax)

;; * Defining the data structures
;;
;; A puzzle consists of 81 fields (9x9) where each field can take a value from 1
;; to 9. Blocked fields are encoded using =10= and blocked values negative
;; integers.

;; ** A new type for the legal values of a field.
(deftype value ()
  "A new type for the possible values."
  '(integer -9 10))

;; ** A class for the puzzle
;;
;; I am using a class for the puzzle holding the current puzzle grid to solve
;; and an array of the possible values.  To save space the possible values are
;; stored using the bits of an nine bit integer.
(defclass puzzle ()
  ((grid :documentation "The current str8ts grid."
	 :initform (make-array '(9 9)
			       :element-type 'value
			       :initial-element 0)
	 :initarg :grid
         :accessor grid)
   (digits :documentation "All possible values for each field, as bits."
	   :initform (make-array '(9 9)
				 :element-type '(integer 0 #b111111111)
				 :initial-element #b111111111)
	   :initarg :digits
           :accessor digits)))

;; A helper function for printing the bit fields
(defun bits (value-field)
  "Prints the value field as nine digits bit field."
  (format nil "~9,'0b" value-field))

;; *** The units and sub-units of the current puzzle

(defvar *units* (make-array '(9 9) :element-type 'list :initial-element nil)
  "The list of all possible units in a str8ts grid.")

(defvar *sub-units* (make-array '(9 9) :element-type 'list :initial-element nil)
  "list of all possible sub-units in a str8ts grid.")

;; *** Error conditions
;;
;; To track contradictions I define a condition.
(define-condition unit-contains-contradictory-solution (condition) ())

;; * The generic functions
;;
;; I define generic function even if the methods have only one purpose.
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

(defgeneric find-field-with-fewest-possibilities (puzzle)
  (:documentation "Returns a sorted list of the counts of possible values (count (row . col))."))

(defgeneric valid-puzzle-p (puzzle)
    (:documentation "Returns T if the un-solved puzzle is still valid."))

(defgeneric search-puzzle (puzzle step)
  (:documentation "Using depth-first search and propagation, try all possible values."))

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

(defun only-possible-value-is-p (possible-digits value)
  "Returns T vaue found in POSSIBLE-DIGITS is VALUE."
  (and (logbitp (- value 1) possible-digits)
       (= 1 (logcount possible-digits))))

(defun list-all-possible-digits (possible-digits)
  "Returns a list of all possible digits to explore."
  (loop for i from 1 to 9
     when (logbitp (- i 1) possible-digits)
     collect i))

(defun value-is-set-p (possible-digits value)
  "Returns T if VALUE is possible in POSSIBLE-DIGITS."
  (logbitp (- value 1) possible-digits))

(defun unset-possible-value (possible-digits value)
  "Returns an integer representing POSSIBLE-DIGITS with VALUE unset"
  (logxor possible-digits (ash 1 (- value 1))))

;; * Constrained propagation
;;
;; *** Eliminating the impossible digits
;;
(defmethod eliminate-value-in-units ((puzzle puzzle) row col value)
  (iter
    (for u in (aref *units* row col))
    (log:debug "R: ~D C: ~D ~A" row col u)
    (iter
      (for (r . c) in u)
      (eliminate puzzle r c value))))

(defmethod list-places-with-single-unit-solution ((puzzle puzzle) row col value)
  (with-slots (digits) puzzle
    (iter
      (for unit in (aref *units* row col))
      (nconcing
       (destructuring-bind (n positions)
	   (iter
             (for (r . c) in unit)
	     (when (only-possible-value-is-p (aref digits r c) value)
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
    ;; Check if the value is already unset
    (when (value-is-set-p (aref digits row col) value)
      ;; Eliminate the value from the set of possible digits
      (let ((possible-digits
              (unset-possible-value (aref digits row col) value)))
        (setf (aref digits row col) possible-digits)
        ;; Now if we're left with a single possible value
        (when (and (<= 0 (aref grid row col)) (= 1 (count-remaining-possible-digits possible-digits)))
          (let ((found-value (first-set-value possible-digits)))
            ;; Update the main grid
            (setf (aref grid row col) found-value)
            ;; Eliminate that value we just found in all peers
            (eliminate-value-in-units puzzle row col found-value)
            ))
        ;; Now check if any unit has a single possible place for that value
        (iter (for (r . c) in (list-places-with-single-unit-solution puzzle row col value))
              (assign puzzle r c value))))))

;; *** Assigning a value
(defmethod assign ((puzzle puzzle) row col value)
  (with-slots (grid digits) puzzle
    ;; When 1 <= v <= 9 --> Given value, eliminate the other digits for this field
    (setf (aref grid row col) value)
    (iter
      (for other-value from 1 to 9) ; Eliminate the others digits
      (unless (= other-value value)
        (eliminate puzzle row col other-value)))
    ;; Eliminate the value from the units of field.
    (iter (for unit in (aref *units* row col))
          (iter (for (r . c) in unit)
                (eliminate puzzle r c value))))
  puzzle)

;; * Read in a puzzle

(defmethod read-grid ((puzzle puzzle) &optional (file #p"puzzles/2019-01-26-medium"))
  ;; A puzzle consists of 81 fields (9x9) where each field can take a value from =1=
  ;; to =9=. Blocked fields are encoded using =10= and blocked values negative
  ;; integers.
  ;;
  ;; The puzzle is given as one line per file. 
  (with-slots (grid digits) puzzle
    ;; Filling the grid and determining the units/sub-units
    (iter
      (for v in-file file)
      (for i from 0)
      (setf (row-major-aref grid i) v))
    (setq *units* (puzzle-units puzzle)
          *sub-units* (puzzle-sub-units puzzle))
    ;; When the units are known assigning the digits.
    (iter
      (for i below 81)
      (for v = (row-major-aref grid i))
      (for (values r c) = (floor i 9))
      (when (<= -9 v -1)
        ;; When -9 <= v <= -1 --> blocked field, remove v from the units of the field
        (setf (aref digits r c) 0)
        (eliminate-value-in-units puzzle r c (abs v)))
      (when (<= 1 v 9)
        ;; Only when a proper value is given assign it.
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
       (unless (or (= 10  (aref grid row pc))
                   (minusp (aref grid row pc))) ; Blocked and empty fields
         (collect (cons row pc))))

     ;; unit with peers on the same column
     (iter
       (for pr below 9)
       (unless (or (= 10 (aref grid pr col))
                   (minusp (aref grid pr col))) ; Blocked and empty fields
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

;; ** Generate the units and sub-units of the whole puzzle

(defmethod puzzle-units ((puzzle puzzle))
  (with-slots (grid) puzzle
    (iter
      (with units = (make-array '(9 9) :element-type 'list :initial-element nil))
      (for row below 9)
      (iter
        (for col below 9)
        (unless (= 10 (aref grid row col))
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
        (unless (or (= 10 #1=(aref grid row col)) (minusp #1#))
          (setf (aref sub-units row col)
                (multiple-value-bind (su-row su-col)
                    (find-sub-units puzzle row col)
                  (remove-if-not (curry #'in-sub-unit-p row col)
                                 (append su-row su-col))))))
      (finally (return sub-units)))))

;; ** Create a puzzle object.
(defun make-puzzle (&optional (file #p"puzzles/2019-01-26-medium"))
  "Make a str8ts puzzle from the given FILE.

A 10 indicates a black field, negative numbers are numbers within black fields,
the given numbers and the empty fields are represented by 0."
  (let ((p (make-instance 'puzzle)))
    (read-grid p file)
    p))

;; * Printing the puzzle
(defmethod print-puzzle ((puzzle puzzle) i)
  (with-slots (grid) puzzle
    (let ((size (array-dimension grid 0)))
      (cond ((equal i 0) (format t "~&Initial puzzle:~%"))
	    ((numberp i) (format t "~%~% Round: ~D~%" i))
	    (t (format t "~&Final state:~%")))
      (format t " ~v@{~A~:*~}~%" (1- (* 6 size)) "-")
      (iter 
        (for i below size)
        (iter 
          (for j below size)
          (format t "| ~3D " (aref grid i j)))
        (format t "|~%"))
      (format t " ~v@{~A~:*~}~%" (1- (* 6 size)) "-"))
    i))   ; Returns the current step
 
;; * The predicates
;;
;; ** Is the puzzle solved?

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
           ;; Because I have to scan for the minimum I will determine the
           ;; length in the same task.
           (iter
             (with min = 11)            ; The maximum value should be 10
             (for i from 0)
             (for obj in sub-unit)
             (when (and (not (zerop obj)) (< obj min))
               (setf min obj))
             (finally (return (values i min))))))
    (multiple-value-bind (len min)
        (sub-unit-aux sub-unit)
      (iter
        (with indicator = (make-array len :element-type 'bit :initial-element 0))
        (for obj in sub-unit)
        (when (>= (- obj min) len)
          ;; obj out of range
          (return-from valid-subunit-p nil))
        (cond
          ((zerop obj)
           ;; obj is 0, check next obj
           nil)
          ((zerop (aref indicator (- obj min)))
           ;; obj is being seen for the 1st time; record that
           (setf (aref indicator (- obj min)) 1))
          (t
           ;; duplicate obj
           (return-from valid-subunit-p nil)))) 
      ;; all list elements are unique and in the right range;
      ;; injectivity + same cardinality for finite sets => surjectivity
      t)))

(defmethod valid-puzzle-p ((puzzle puzzle))
  (with-slots (grid) puzzle
    (every #'valid-subunit-p
           (iter
             (for i below 81)
             (for su = (row-major-aref *sub-units* i))
             (appending
              (iter
                (for u in su)
                (collect
                    (iter
                      (for (r . c) in u)
                      (collect (aref grid r c))))))))))

;; * Solving the puzzle

;; ** Depth-first search

;; I will not implement further logic to reduce the number of possible values
;; but try and check them all.
(defmethod find-field-with-fewest-possibilities ((puzzle puzzle))
  (with-slots (digits) puzzle
    (destructuring-bind (n (row . col))
	(first
	 (sort
	  (iter
            (for r below 9)
	    (nconcing (iter
                        (for c below 9)
		        (for n = (count-remaining-possible-digits (aref digits r c)))
		        (when (< 1 n) 
		          (collect (list n (cons r c)))))))
	  (lambda (a b)
	    (< (car a) (car b)))))
      @ignore n
      (cons row col))))

(defmethod search-puzzle ((puzzle puzzle) step)
  (cond
    ((null puzzle) nil)                 ; Earlier failure
    ((not (valid-puzzle-p puzzle)) nil) ; Invalid puzzle
    ((solvedp puzzle) puzzle)           ; Solved
    (t					; Search
     ;; Chose the unfilled field with the fewest possibilities
     (log:debug "Level: ~D" step)
     (with-slots (digits) puzzle
       (destructuring-bind (row . col)
	   (find-field-with-fewest-possibilities puzzle)
	 (some
	  (lambda (candidate)
	    (handler-case               ; Skip search errors and continue
	        (search-puzzle (assign (copy-puzzle puzzle) row col candidate) (1+ step))
	      (unit-contains-contradictory-solution () nil)))
	  (list-all-possible-digits (aref digits row col))))))))

;; ** Solving a given puzzle

;; *** Determine the timing

;; Calculates how long the evaluation of the body forms, i.e. the solving of
;; the puzzle takes.
;; Taken from https://github.com/dimitri/sudoku
(defmacro timing (&body forms)
  "Return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values ,result (/ (- ,end ,start) internal-time-units-per-second)))))


(defmethod solve-puzzle (file)
  "Solves the puzzle given in FILE.

See function read-grid for the format."
  (let ((p (make-puzzle file)))
    (print-puzzle p 0)
    (multiple-value-bind (puzzle time)
        (timing (search-puzzle p 0))
      (print-puzzle puzzle t)
      (format t "Puzzle solved in ~,3F seconds." time))))


