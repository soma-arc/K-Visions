;; (provide "sl2c")
;; (defpackage "sl2c"
;;   (:use #:cl)
;;   (:export #:sl2c
;; 	   #:prod
;; 	   #:inverse))
;; (in-package "sl2c")

(locally (declare (optimize (speed 3) (debug 0) (safety 0)))
  (defvar +infinity+ (* most-positive-fixnum #c (1 1)))

  (defstruct (sl2c (:print-object print-sl2c))
    (a 1 :type (complex double-float))
    (b 0 :type (complex double-float))
    (c 0 :type (complex double-float))
    (d 1 :type (complex double-float)))

  (defun print-sl2c (sl2c stream)
    (format stream "{~d ~d~%~d ~d}~%" (sl2c-a sl2c) (sl2c-b sl2c) (sl2c-c sl2c) (sl2c-d sl2c)))

  (declaim (inline prod)
	   (inline inverse))
  (defun prod (m1 m2)
    (declare (type sl2c m1 m2))
    (the sl2c (make-sl2c :a (+ (* (sl2c-a m1) (sl2c-a m2)) (* (sl2c-b m1) (sl2c-c m2)))
			 :b (+ (* (sl2c-a m1) (sl2c-b m2)) (* (sl2c-b m1) (sl2c-d m2)))
			 :c (+ (* (sl2c-c m1) (sl2c-a m2)) (* (sl2c-d m1) (sl2c-c m2)))
			 :d (+ (* (sl2c-c m1) (sl2c-b m2)) (* (sl2c-d m1) (sl2c-d m2))))))

  (defun inverse (sl2c)
    (declare (type sl2c sl2c))
    (let ((determinant (- (* (sl2c-a sl2c) (sl2c-d sl2c))
			  (* (sl2c-b sl2c) (sl2c-c sl2c)))))
      (declare (type double-float determinant))
      (if (= determinant 0)
	  (the sl2c (make-sl2c :a +infinity+
			       :b +infinity+
			       :c +infinity+
			       :d +infinity+))
	  (the sl2c (make-sl2c :a (/ (sl2c-d sl2c) determinant)
			       :b (/ (* -1 (sl2c-b sl2c)) determinant)
			       :c (/ (* -1 (sl2c-c sl2c)) determinant)
			       :d (/ (sl2c-a sl2c) determinant))))))

  (defun sl2c-trace (sl2c)
    (declare (type sl2c sl2c))
    (the (complex double-float) (+ (sl2c-a sl2c) (sl2c-d sl2c)))))
