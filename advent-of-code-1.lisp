(ql:quickload 'cl-utilities)
(ql:quickload 'iterate) 
(defpackage :com.thoriumnext.bunny
  (:use :common-lisp :iterate)
  (:import-from :cl-utilities :split-sequence))

(defparameter bunny-input
  "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3")


(defconstant start-state (list #C(0 0) ; position
			       #C(0 1) ; orientation
			       ))

(defun chop-input (str)
  (split-sequence #\, (remove #\Space str)))

(defun turn-go-pair<-str (str)
  "Converts a string to a pair of 
- a complex number (direction to turn) and 
- a real positive integer (distance to go)."
  (list (if (string= "R" (subseq str 0 1))
	    #C(0 -1)
	    #C(0 1))
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

(defun reduce-input (parsed-input)3
  "Returns a pair of
- position (complex number) and 
- orientation (complex number)"
  (reduce #'next-pos parsed-input :initial-value start-state)) 

(defun new-yawk-distance (final-position)
  (+ (abs (realpart final-position)) (abs (imagpart final-position))))

;;; Simple, because no state is needed except for that stored by reduce.
(defun hq-distance (input)
  (new-yawk-distance (car (reduce-input (parse-input input)))))

;;; Need to examine each step, and maintain state (what has been visited, where am I, which way am I oriented, how far have I gone in this leg).
(defun hq-distance-2 (input)
  (let ((second-visit)
	(positions-visited (make-hash-table))
	(position 0)
	(direction #C(0 1)))
    (setf (gethash 0 positions-visited) t) 
    (loop
       for leg in (parse-input input)
       do (destructuring-bind (turn distance) leg
	    (setf direction (* direction turn))
	    (loop 
	       for i from 1 to distance
	       do (setf position (+ position direction))
;		 (print (list :position :x (realpart position) :y (imagpart position) :visited-before (gethash position positions-visited)))

		 (if (gethash position positions-visited)
		     (setf second-visit t)
		     (setf (gethash position positions-visited) t))
		 
		 
	       until second-visit))
       until second-visit
       finally (return (new-yawk-distance position)))))




(defun foo (visiteds position-orientation-pair turn-distance-pair)
  (destructuring-bind (position orientation turn distance)
      (append position-orientation-pair turn-distance-pair)
;;    (print (list :position position
;;		 :orientation orientation
;;		 :td turn-distance-pair
;;		 :facing (* orientation turn)
;;		 :visiteds visiteds))
;;   (setf (gethash #C(-5 0) visiteds) t)   
    (loop
       with direction = (* orientation turn)
       with new-pos = position
       for i from 1 to distance
       do
	 (setf new-pos (+ position (* i direction)))
	 (if (gethash new-pos visiteds)
	     (return (list :second-time t :pos new-pos))
	     (print (list :new-pos new-pos
			  :pos-added (setf (gethash new-pos visiteds) t))))
       finally (return (list :second-time nil :pos new-pos)))
    ))




(defun gt-take (n lst)
  (subseq lst 0 n))

(defconstant bunny-input2
  "R8, R4, R4, R8")



(defun gt-take (n lst)
  (subseq lst 0 n))



;; Given a turn-dist (and having a current position, current orientation, and a set of positions that have been visited, return either a success;; (:SUCCESS t :POS pos) or a not-yet (:SUCCESS :FALSE).
(let ((visited-positions (make-hash-table)))
  (loop for instruction in (parse-input bunny-string)
     do (let ((one-step (* orientation turn)))
	  (if (eq (getf :success
			(loop for i from 1 to (second instruction)
			   with next-step = (+ latest-visited-position one-step)
			   do (if (gethash next-step visited-positions)
				  (return (list :success t :position next-position))
					; else
				  (setf (gethash next-position visited-positions) t)))&rest

				  
       

				  
				  with next-position = (+ latest-visited-position one-step)
				     do (if (gethash next-position visited-positions)
				

;; Given a position (and having a current position, current orientation, and a set of positions that have been visited) return either a success and  or a not-yet.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun new-dir (facing turn)
  (* facing (if (eq turn #\R) #C(0 1) #C(0 -1))))

(defun new-pos (pos new-leg)
  (+ pos new-leg))

(defun new-state (state change)
  (destructuring-bind ((pos facing) (turn dist)) (state change)
    (let ((new-facing (new-dir facing turn)))
      (list (+ pos (* new-facing dist)) new-facing))))

(ql:quickload :cl-utilities)

(defun decode-input (str)
  (mapcar (lambda (step) (list (if (eq #\R (intern (char step 0)))
				   #C(0 1)
				   #C(0 -1))))
	  (parse-integer (subseq str 1))))





(defparameter directions (list 0 1 2 3))

(defparameter origin '(:NS 0 :EW 0 :DIR 0))

(defun new-pos (pos dir dist)
  (case dir
    (0 (list (+ (first pos) dist) (second pos)))
    (1 (list (first pos) (+ dist (second pos))))
    (2 (list (- (first pos) dist) (second pos)))
    (3 (list (first pos) (- dist (second pos))))))


    

(defparameter dirs '((0 1) (-1 0) (0 -1) (1 0)))



