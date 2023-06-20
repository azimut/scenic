(in-package #:scenic)

(defclass scene-ssao (scene ssao)
  ())

(defun make-scene-ssao (&rest args)
  (apply #'make-instance 'scene-ssao args))
