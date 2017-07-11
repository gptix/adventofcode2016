(ql:quickload 'cl-utilities)
(ql:quickload 'iterate) 
(defpackage :com.thoriumnext.bunny
  (:use :common-lisp :iterate)
  (:import-from :cl-utilities :split-sequence))

(defparameter *bunny-input*
  "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3")


(defconstant +north+ #C(0 1))
(defconstant +origin+ #C(0 0))
(defconstant +initial-state+ (list +origin+ +north+))
(defconstant +R+ #C(0 -1))
(defconstant +L+ #C(0 1))

(defun chop-input (str)
  (split-sequence #\, (remove #\Space str)))

(defun turn-go-pair<-str (str)
  "Converts a string to a pair of 
- a complex number (direction to turn) and 
- a real positive integer (distance to go)."
  (list (if (string= "R" (subseq str 0 1))
	    +R+
	    +L+)
	(parse-integer (subseq str 1))))
	
(defun parse-input (str)
  (mapcar #'turn-go-pair<-str (chop-input str)))
 
(defun next-pos (current-state turn-go-pair)
  "Given a position, orientation, turn direction, and distance, calculate a new state (position and orientation)."
  (destructuring-bind (pos orientation turn go)
      (append current-state turn-go-pair)
    (let ((new-orientation (* orientation turn)))
      (list
       (+ (* new-orientation go) pos) ; new pos
       new-orientation))))

(defun reduce-input (parsed-input)
  "Returns a pair of
- position (complex number) and 
- orientation (complex number)"
  (reduce #'next-pos parsed-input :initial-value +initial-state+)) 

(defun new-yawk-distance (final-position)
  (+ (abs (realpart final-position)) (abs (imagpart final-position))))

;;; Simple, because no state is needed except for that stored by reduce.
(defun hq-distance (input)
  (new-yawk-distance (car (reduce-input (parse-input input)))))

(hq-distance *bunny-input*)

;;; Need to examine each step, and maintain state (what has been visited, where am I, which way am I oriented, how far have I gone in this leg).
(defun hq-distance-2 (input)
  (let ((second-visit)
	(positions-visited (make-hash-table))
	(position +origin+)
	(direction +north+))
    (setf (gethash 0 positions-visited) t) 
    (loop
       for leg in (parse-input input)
       do (destructuring-bind (turn distance) leg
	    (setf direction (* direction turn))
	    (loop 
	       for i from 1 to distance
	       do (setf position (+ position direction))
		 (if (gethash position positions-visited)
		     (setf second-visit t)
		     (setf (gethash position positions-visited) t))
	       until second-visit))
       until second-visit)
    (new-yawk-distance position)))

(hq-distance-2 *bunny-input*)
