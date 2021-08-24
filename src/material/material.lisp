(in-package #:scenic)

(defclass material ()
  ((specular  :accessor specular  :initarg :specular)
   (metallic  :accessor metallic  :initarg :metallic)
   (emissive  :accessor emissive  :initarg :emissive)
   (roughness :accessor roughness :initarg :roughness)
   (uploadp   :accessor uploadp   :initarg :uploadp)
   (idx       :reader   idx)
   (ubo       :reader   ubo))
  (:default-initargs
   :uploadp   T
   :specular  0.2
   :metallic  0.3
   :emissive  0.0
   :roughness 0.3)
  (:metaclass counted-class)
  (:documentation "pbr material"))

(defun make-material (&rest args)
  (apply #'make-instance 'material args))

(defmethod print-object ((obj material) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (specular metallic emissive roughness idx) obj
      (format stream "IDX:~a R:~a E:~a M:~a S:~a"
              idx roughness emissive metallic specular))))

(defmethod (setf specular)  :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf metallic)  :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf emissive)  :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf roughness) :after (_ (obj material)) (setf (uploadp obj) T))

(defun reset-material-counter ()
  (setf (slot-value (find-class 'material) 'counter) 0))
(defun current-material-counter ()
  (slot-value (find-class 'material) 'counter))

;; NOTE: can't upload here, as the UBO is not allocated yet.
(defmethod initialize-instance :after ((obj material) &key)
  (setf (slot-value obj 'idx) (current-material-counter)))

(defstruct-g (pbr-material :layout :std-140)
  (specular  (:float 5))
  (metallic  (:float 5))
  (emissive  (:float 5))
  (roughness (:float 5)))

(defmethod upload ((obj material))
  (with-slots (ubo idx specular metallic emissive roughness) obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (aref-c (pbr-material-specular  (aref-c c 0)) idx) specular)
      (setf (aref-c (pbr-material-metallic  (aref-c c 0)) idx) metallic)
      (setf (aref-c (pbr-material-emissive  (aref-c c 0)) idx) emissive)
      (setf (aref-c (pbr-material-roughness (aref-c c 0)) idx) roughness))))
