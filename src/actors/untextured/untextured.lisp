(in-package #:scenic)

(defclass untextured (actor)
  ()
  (:documentation "pbr untextured"))

(defun make-box (&optional (w 1f0) (h 1f0) (d 1f0) &rest initargs)
  (apply #'make-instance 'untextured :buf (box w h d) initargs))
(defun make-sphere (&optional (radius 1f0) (lat 30) (long 30) &rest initargs)
  (apply #'make-instance 'untextured :buf (sphere radius lat long) initargs))
(defun make-cylinder (&optional (radius 0.5) (height 1f0) &rest initargs)
  (apply #'make-instance 'untextured :buf (cylinder radius height) initargs))
(defun make-cone (&optional (radius 1f0) (height 1f0) &rest initargs)
  (apply #'make-instance 'untextured :buf (cone radius height) initargs))

