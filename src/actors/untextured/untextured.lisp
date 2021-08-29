(in-package #:scenic)

(defclass untextured (actor)
  ()
  (:documentation "pbr untextured"))

(defun make-box (&key (w 1f0) (h 1f0) (d 1f0) (pos (v! 0 0 0)) (material 0))
  (make-instance 'untextured :buf (box w h d) :pos pos :material material))
(defun make-sphere (&key (radius 1f0) (lat 30) (long 30) (pos (v! 0 0 0)) (material 0))
  (make-instance 'untextured :buf (sphere radius lat long) :pos pos :material material))
(defun make-cylinder (&key (radius 0.5) (height 1f0) (pos (v! 0 0 0)) (material 0))
  (make-instance 'untextured :buf (cylinder radius height) :pos pos :material material))
(defun make-cone (&key (radius 1f0) (height 1f0) (pos (v! 0 0 0)) (material 0))
  (make-instance 'untextured :buf (cone radius height) :pos pos :material material))

