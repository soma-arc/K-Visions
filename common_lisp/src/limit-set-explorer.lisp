(load "mobius.lisp")

(locally
    (declare (optimize (speed 3) (debug 0) (safety 0)))
;;    (declare (optimize (speed 3) (safety 2)))
  (defstruct (limit-set-explorer
	       (:constructor create-limit-set-explorer
			     (max-level threshold gens
					&aux (words
					      (make-array (+ max-level 1)
							  :element-type 'sl2c
							  :initial-element (make-sl2c)))
					(tags
					 (make-array (+ max-level 1)
						     :element-type 'fixnum
						     :initial-element -1)))))
    (max-level 1 :type fixnum)
    (level 1 :type fixnum)
    (threshold 1 :type double-float)
    (words nil :type (simple-array 'sl2c (*)))
    (tags nil :type (simple-array 'fixnum (*)))
    (gens nil :type (simple-array 'sl2c (4)))
    )
  (declaim (inline go-forward)
	   (inline go-backward)
	   (inline available-turn-p)
	   (inline branch-termination-p)
	   (inline turn-and-go-forward))

  (defun get-fix-points (gens)
    (declare (type (simple-array sl2c (4)) gens))
    (let ((a (aref gens 0))
	  (b (aref gens 1))
	  (_A (aref gens 2))
	  (_B (aref gens 3))
	  (fix-points (make-array '(4 3)
				  :element-type 'complex)))
      (setf (aref fix-points 0 0) (fix-point-plus (reduce #'prod (list b _A _B a))))
      (setf (aref fix-points 0 1) (fix-point-plus a))
      (setf (aref fix-points 0 2) (fix-point-plus (reduce #'prod (list _B _A b a))))

      (setf (aref fix-points 1 0) (fix-point-plus (reduce #'prod (list _A _B a b))))
      (setf (aref fix-points 1 1) (fix-point-plus b))
      (setf (aref fix-points 1 2) (fix-point-plus (reduce #'prod (list  a _B _A b))))

      (setf (aref fix-points 2 0) (fix-point-plus (reduce #'prod (list _B a b _A))))
      (setf (aref fix-points 2 1) (fix-point-plus _A))
      (setf (aref fix-points 2 2) (fix-point-plus (reduce #'prod (list b a _B _A))))

      (setf (aref fix-points 3 0) (fix-point-plus (reduce #'prod (list a b _A _B))))
      (setf (aref fix-points 3 1) (fix-point-plus _B))
      (setf (aref fix-points 3 2) (fix-point-plus (reduce #'prod (list _A b a _B))))
      fix-points))

  (defun calc-limit-set (gens max-level threshold)
    (declare
     (type fixnum max-level)
     (type double-float threshold)
     (type (simple-array sl2c (4)) gens))
    (let ((level 1)
	  (fix-points (get-fix-points gens))
	  (words (make-array (+ max-level 1)
			     :element-type 'sl2c
			     :initial-element (make-sl2c)))
	  (tags (make-array (+ max-level 1)
			    :element-type 'fixnum
			    :initial-element -1))
	  (limit-set (make-array (min (* 3 (* 4 (expt 3 10))) (* 3 (* 4 (expt 3 (- max-level 1)))))
				 :element-type 'complex
				 :fill-pointer 0
				 :adjustable t)))
      (declare (type fixnum level)
	       (type (simple-array complex (4 3)) fix-points)
	       (type (simple-array fixnum (*)) tags)
	       (type (simple-array sl2c (*)) words)
	       (type (array sl2c (*)) limit-set))
      (labels ((branch-termination-p ()
;		 (format t "termination ~d~%" level)
		 (let ((z (make-array '(3) :element-type 'complex)))
		   (loop for i from 0 to 2 do
			(setf (aref z i)
			      (on-point (aref words level)
					(aref fix-points (aref tags level) i))))
		   (if (or (= level max-level)
			   (and (<= (abs (- (aref z 1) (aref z 0))) threshold)
				(<= (abs (- (aref z 2) (aref z 1))) threshold)))
		       (progn
			 (loop for i from 0 to 2 do
			      (vector-push-extend (aref z i) limit-set))
			 t)
		       nil)))

	       (available-turn-p ()
;		 (format t "available-turn ~d~%" level)
		 (let ((t1 (mod (+ (aref tags level) 2) 4))
		       (t2 (mod (- (aref tags (+ level 1)) 1) 4)))
		   (if (= t1 t2)
		       nil
		       t)))

	       (go-forward ()
;		 (format t "forward ~d~%" level)
		 (if (eq (branch-termination-p) nil)
		     (progn
		       (incf level)
		       (setf (aref tags level)
			     (mod (+ (aref tags (- level 1)) 1) 4))
		       (setf (aref words level)
			     (prod (aref words (- level 1)) (aref gens (aref tags level))))
		       (go-forward))
		     ))

	       (go-backward ()
;		 (format t "back ~d~%" level)
		 (decf level)
		 (if (and (/= level 0) (not (available-turn-p)))
		     (go-backward)))

	       (turn-and-go-forward ()
;		 (format t "turn and go ~d~%" level)
		 (setf (aref tags (+ level 1))
		       (mod (- (aref tags (+ level 1)) 1) 4))
		 (if (= level 0)
		     (setf (aref words 1) (aref gens (aref tags 1)))
		     (setf (aref words (+ level 1))
			   (prod (aref words level)
				 (aref gens (aref tags (+ level 1))))))
		 (incf level))
	       (explore ()
;		 (format t "explore ~d~%" level)
		 (go-forward)
		 (go-backward)
		 (turn-and-go-forward)
		 (if (or (/= level 1) (/= (aref tags 1) 1))
		     (explore))))
	(setf (aref tags 1) 1)
	(setf (aref words 1) (aref gens 1))
	(explore)
	limit-set))))
