(load "sl2c.lisp")
;; (provide "mobius-util")
;; (require "sl2c")
;; (defpackage "mobius-util"
;;   (:use :cl
;; 	:sl2c))

(locally
    (declare (optimize (speed 3) (debug 0) (safety 0))
	     (inline (fix-point-plus))
	     (inline (on-point)))

  (defun on-point (m p)
    (declare (type sl2c m)
	     (type (complex double-float) p))
    (if (= +infinity+ (sl2c-c m))
	(if (= (sl2c-c m) 0)
	    +infinity+
	    (/ (sl2c-a m) (sl2c-c m)))
	(let ((numerator (+ (* (sl2c-a m) p) (sl2c-b m)))
	      (denominator (+ (* (sl2c-c m) p) (sl2c-d m))))
	  (if (= denominator 0)
	      +infinity+
	      (/ numerator denominator)))))

  (defun fix-point-plus (m)
    (declare (type sl2c m))
    (if (= (sl2c-c m) 0)
	+infinity+
	(/ (+ (- (sl2c-a m) (sl2c-d m)) (sqrt (- (expt (sl2c-trace m) 2) 4)))
	   (* (sl2c-c m) 2))))

  (defun parabolic-commutator-recipe (t_a t_b t_ab-plus-p)
    (declare (type (complex double-float) t_a t_b))
    (let* ((t_ab (if t_ab-plus-p
		     (/ (+ (* t_a t_b)
			   (sqrt (- (* (expt t_a 2) (expt t_b 2))
				    (* 4 (+ (expt t_a 2) (expt t_b 2))))))
			2)
		     (/ (- (* t_a t_b)
			   (sqrt (- (* (expt t_a 2) (expt t_b 2))
				    (* 4 (+ (expt t_a 2) (expt t_b 2))))))
			2)))
	   (z0 (/ (* (- t_ab 2) t_b)
		  (+ (- (* t_b t_ab) (* t_a 2))
		     (* t_ab #c(0 2)))))
	   (a (make-sl2c :a (/ t_a 2)
			 :b (/ (+ (- (* t_a t_ab) (* t_b 2)) #c(0 4))
			       (* z0 (+ (* t_ab 2) 4)))
			 :c (/ (* z0
				  (- (* t_a t_ab) (* t_b 2) #c(0 4)))
			       (- (* t_ab 2) 4))
			 :d (/ t_a 2)))
	   (b (make-sl2c :a (/ (- t_b #c(0 2)) 2)
			 :b (/ t_b 2)
			 :c (/ t_b 2)
			 :d (/ (+ t_b #c(0 2)) 2)))
	   (gens (make-array 4 :element-type 'sl2c)))
      (setf (aref gens 0) a)
      (setf (aref gens 1) b)
      (setf (aref gens 2) (inverse a))
      (setf (aref gens 3) (inverse b))
      (the (simple-array sl2c (4)) gens))))
