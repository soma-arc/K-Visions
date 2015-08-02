(load "limit-set-explorer.lisp")

(time (defparameter *limit-set* (calc-limit-set (parabolic-commutator-recipe -2 -2 t) 35 0.004d0)))


(ql:quickload "cl-glut")
(defparameter *width* 500)
(defparameter *height* 500)
(defparameter *magnification* 100)
(defclass my-window (glut:window)
  ()
  (:default-initargs :title "limit set" :width *width* :height *height*
		     :mode '(:single :rgb :depth)))

(defmethod glut:display-window :before ((w my-window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *width* *height* 0 -1 1))

(defun set-line-vertexes (lines)
  (loop for point across lines do
       (gl:vertex (* *magnification* (realpart point))
		  (* *magnification* (imagpart point))
		  0)))

(defmethod glut:display ((window my-window))
  (gl:clear :color-buffer-bit)
  (%gl:color-3f 1 1 1)
  (gl:push-matrix)
  (gl:translate (/ *width* 2) (/ *height* 2) 0)
  (gl:begin :line-strip)
  (set-line-vertexes *limit-set*)
  (gl:end)
  (gl:pop-matrix)
  (gl:flush))

(defun draw-limit-set ()
  (glut:display-window (make-instance 'my-window)))

(draw-limit-set)
