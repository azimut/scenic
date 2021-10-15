(in-package #:scenic)

(defclass untextured (actor)
  ()
  (:documentation "pbr untextured"))

(defun make-box (&rest initargs &key (w 1f0) (h 1f0) (d 1f0) &allow-other-keys)
  (remf initargs :d) (remf initargs :w) (remf initargs :h)
  (apply #'make-instance 'untextured :buf (box w h d) initargs))
(defun make-sphere (&rest initargs &key (radius 1f0) (lat 30) (long 30) &allow-other-keys)
  (apply #'make-instance 'untextured :buf (sphere radius lat long) initargs))
(defun make-cylinder (&rest initargs &key (radius 0.5) (height 1f0) &allow-other-keys)
  (apply #'make-instance 'untextured :buf (cylinder radius height) initargs))
(defun make-cone (&rest initargs &key (radius 1f0) (height 1f0) &allow-other-keys)
  (apply #'make-instance 'untextured :buf (cone radius height) initargs))

