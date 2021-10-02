(in-package #:scenic)

(defclass rabbit (untextured)
  ())

(defmethod initialize-instance :after ((obj rabbit) &key)
  (destructuring-bind (&key buf &allow-other-keys)
      (first (assimp-load-meshes "/home/sendai/projects/cl/incandescent/static/bunny.obj"))
    (setf (slot-value obj 'buf) buf)))

(defun make-rabbit (&rest args)
  (apply #'make-instance 'rabbit args))
