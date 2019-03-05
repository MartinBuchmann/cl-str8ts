;; -*- ispell-local-dictionary: "en_GB" -*-
;; Time-stamp: <2019-03-05 21:02:27 Martin>
;; * str8ts-draw.lisp
;;
(in-package #:str8ts)
(annot:enable-annot-syntax)

;; Adapt this to your needs...
(defvar *font* "~/Library/Fonts/VeraMono.ttf")
(defvar *width* 900)
(defvar *height* *width*)

(defmethod draw-puzzle ((puzzle puzzle) file)
  (with-canvas (:width *width* :height *height*)
    (let* ((font (get-font *font*))
           (step (/ *width* 9))
           (x-max (- *width* step))
           (y-max (- *height* step)))
      ;; Basic parameters
      (set-font font (* 0.75 step))
      (set-rgb-stroke 0 0 0)
      ;; The frame
      (rectangle 0 0 *width* *height*)
      (set-line-width 6)
      (stroke)
      ;; The vertical lines
      (iter (for x from step to x-max by step)
            (move-to x 0)
            (line-to x *height*)
            (set-line-width 4)
            (stroke))
      ;; The horizontal lines
      (iter (for y from step to y-max by step)
            (move-to 0 y)
            (line-to *width* y)
            (set-line-width 4)
            (stroke))
      ;; The data
      (with-slots (grid) puzzle
        (iter
          (for col from 0 to 8)
          (for x from 0 to x-max by step)
          (iter
            (with x-offset = (* step 0.5))
            (with y-offset = (* step 0.25))
            (for row from 8 downto 0)
            (for y from 0 to y-max by step)
            (for v = (aref grid row col))
            (cond ((<= 1 v 9)
                   (set-rgb-fill 0 0 0)
                   (draw-centered-string (+ x x-offset) (+ y y-offset) (write-to-string v)))
                  ((<= -9 v -1)
                   (rectangle x y step step)
                   (set-rgb-fill 0 0 0)
                   (fill-path)
                   (set-rgb-fill 1 1 1)
                   (draw-centered-string (+ x x-offset) (+ y y-offset) (write-to-string (abs v))))
                  ((= 10 v)
                   (rectangle x y step step)
                   (set-rgb-fill 0 0 0)
                   (fill-path))))))
      (save-png file))))
