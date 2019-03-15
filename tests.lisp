(in-package :str8ts)

;; * Testing using prove
(prove:plan 47)

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

;; ** Testing the bit representations
(prove:is (count-remaining-possible-digits '#b000000001) 1)
(prove:is (count-remaining-possible-digits '#b101010100) 4)

(prove:is (first-set-value '#b010000000) 8)

(prove:ok (only-possible-value-is-p '#b010000000 8))
(prove:ok (not (only-possible-value-is-p '#b001000000 8)))

(prove:is (list-all-possible-digits '#b010101010) '(2 4 6 8))

(prove:ok (value-is-set-p '#b010000000 8))

(prove:ok (value-is-set-p '#b010000000 8))

(prove:is (unset-possible-value '#b111111111 5) '#b111101111)

(prove:is (integers->digits '(1 2 3 4 5 6 7 8 9)) 511)
(prove:is (integers->digits '(2 3 4 5 6 7 8 9)) 510)
(prove:is (integers->digits '(2 3 4 5 6 7 9)) 382)

;; ** Testing the sub-unit splitting
(prove:is (split-sub-rows '((0 . 0) (0 . 3))) '(((0 . 0)) ((0 . 3))))
(prove:is (split-sub-cols '((0 . 0) (3 . 0))) '(((0 . 0)) ((3 . 0))))

;; ** Testing the data input
(prove:is 81 (array-total-size (grid (make-puzzle))))
(prove:is 81 (array-total-size (grid (make-puzzle #p"puzzles/2019-01-26-solved"))))
(prove:is 81 (array-total-size (grid (make-puzzle #p"puzzles/2019-01-29-hard"))))
(prove:is 81 (array-total-size (grid (make-puzzle #p"puzzles/2019-01-30-easy"))))

;; ** Testing the elimination
(let ((p (make-puzzle))) ; Using the default puzzle
  (prove:is (bits (aref (digits p) 0 0)) "000000010")
  (prove:is (bits (aref (digits p) 5 0)) "111010000")
  (eliminate p 5 0 7)
  (prove:is (bits (aref (digits p) 5 0)) "000010000"))

;; ** Testing the printing
(prove:is 0 (print-puzzle (make-puzzle) 0))
(prove:is t (print-puzzle (make-puzzle #p"puzzles/2019-01-26-solved") t))

;; ** Testing the predicates
(prove:ok (valid-subunit-p '(4 3 1 2)))
(prove:ok (not (valid-subunit-p '(4 3 5 1))))
(prove:ok (valid-subunit-p '(4 3 1 0)))
(prove:ok (valid-subunit-p '(9 0 0 6)))
(prove:ok (valid-subunit-p '(9 0 0 0)))

(prove:ok (solvedp (make-puzzle #p"puzzles/2019-01-26-solved")))
(prove:ok (not (solvedp (make-puzzle))))
(prove:ok (valid-puzzle-p (make-puzzle)))
(prove:ok (valid-puzzle-p (make-puzzle #p"puzzles/2019-01-26-solved")))

(let ((p (make-puzzle)))
  (prove:is (only-valid-digits p 0 0 (integers->digits (list 1 2 3 4 5 6 7 8 9))) '(2) :test #'equalp)
  (prove:is (only-valid-digits p 8 7 (integers->digits (list 1 2 3 4 5 6 7 8 9))) '(7) :test #'equalp)
  (prove:is (only-valid-digits p 3 2 (integers->digits (list 1 2 3 4 5 6 7 8 9))) '(9) :test #'equalp)
  (prove:is (only-valid-digits p 3 6 (integers->digits (list 1 2 3 4 6))) '(4) :test #'equalp)
  (prove:is (only-valid-digits p 3 6 (integers->digits (list 1 2 3 4 6))) '(4) :test #'equalp))

;; ** Testing the search
(prove:is (find-field-with-fewest-possibilities (make-puzzle)) '(4 . 1) :test #'equalp)

(prove:is (grid (make-puzzle #p"puzzles/2019-01-26-solved"))
          (grid (search-puzzle (make-puzzle) 0)) :test #'equalp)
(prove:ok (solve-puzzle "puzzles/2019-01-29-hard"))
(prove:ok (solve-puzzle "puzzles/2019-01-30-easy"))
(prove:ok (solve-puzzle "puzzles/2019-01-28-easy"))
(prove:ok (solve-puzzle "puzzles/2019-01-31-medium"))
(prove:ok (solve-puzzle "puzzles/2019-02-04-hard"))


;; * Reporting the results
(prove:finalize)

