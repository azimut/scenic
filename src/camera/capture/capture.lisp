(in-package #:scenic)

(defclass capture (pers renderable)
  ((ubo     :reader   ubo)
   (uploadp :accessor uploadp :initarg :uploadp)
   (drawp   :accessor drawp   :initarg :drawp))
  (:default-initargs
   :texture-opts '(( 0 :element-type :rgb16f :cubes t)
                   (:d :element-type :depth-component24 :cubes t))
   :sample-opts '((:wrap           :clamp-to-edge
                   :minify-filter  :linear-mipmap-linear
                   :magnify-filter :linear)
                  (:wrap           :clamp-to-edge
                   :minify-filter  :linear
                   :magnify-filter :linear))
   :dim '(128 128)
   :uploadp T
   :drawp T
   :fov 90f0
   :fs (v! 1 1)
   :near .01f0
   :far 100f0)
  (:documentation "cubemap capture of the current scene"))

(defmethod print-object ((obj capture) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos drawp uploadp dim) obj
      (format stream "pos:(~a ~a ~a) dim:(~a ~a) d?:~a u?:~a"
              (x pos) (y pos) (z pos) (first dim) (second dim) drawp uploadp))))

(defmethod (setf near) :after (_ (obj capture)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf pos)  :after (_ (obj capture)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf far)  :after (_ (obj capture)) (setf (uploadp obj) T (drawp obj) T))

(defmethod initialize-instance :after ((obj capture) &key)
  (with-slots (ubo) obj
    (setf ubo (make-ubo NIL 'shadow-projections))
    (upload obj)))

(defmethod free :after ((obj capture))
  (free (ubo obj)))

(defmethod upload ((obj capture))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (shadow-projections-mats (aref-c c 0))
          (projection-mats obj))))

(defun make-capture (&rest args)
  (apply #'make-instance 'capture args))
