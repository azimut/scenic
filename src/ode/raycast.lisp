(in-package #:scenic)

(defclass raycast (uploadable)
  ((from :accessor from :initform :from)
   (to   :accessor to   :initform :to)
   (ray  :reader   ray)
   (hit  :reader   hit))
  (:documentation "ode raycast"))

(defun make-raycast (&rest args)
  (apply #'make-instance 'raycast args))

(defmethod print-object ((obj raycast) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (from to) obj
      (format stream "(~,2f,~,2f,~,2f)->(~,2f,~,2f,~,2f)"
              (x from) (y from) (z from) (x to) (y to) (z to)))))

(defmethod initialize-instance :before ((obj raycast) &key from to)
  (assert (not (v3:= from to))))
(defmethod initialize-instance :after ((obj raycast) &key)
  (setf (slot-value obj 'hit) (cffi:foreign-alloc '%ode:real :count 4))
  (setf (slot-value obj 'ray) (%ode:create-ray *space* 0f0))
  (upload obj))

(defmethod upload ((obj raycast))
  (with-slots (from to ray) obj
    (let ((dir (v3:normalize (v3:- to from))))
      (%ode:geom-ray-set ray (x from) (y from) (z from) (x dir) (y dir) (z dir))
      (%ode:geom-ray-set-length ray (v3:distance to from)))))

(defmethod (setf from) :after (_ (obj raycast)) (setf (uploadp obj) T))
(defmethod (setf to)   :after (_ (obj raycast)) (setf (uploadp obj) T))

(defmethod free ((obj raycast))
  (%ode:geom-destroy (ray obj))
  (cffi:foreign-free (hit obj)))

(cffi:defcallback ray-callback
    :void ((data :pointer) (o1 %ode:geom-id) (o2 %ode:geom-id))
  (cffi-c-ref:c-let ((hit-position %ode:real :count 4 :from data))
    (cffi-c-ref:c-with ((contacts %ode:contact :count 3))
      (dotimes (i (%ode:collide o1 o2 3 (contacts :geom &) (cffi:foreign-type-size '%ode:contact)))
        (when (< (contacts i :geom :depth)
                 (hit-position 3))
          (setf (hit-position 0) (cffi:mem-ref (contacts i :geom :pos) :float 0))
          (setf (hit-position 1) (cffi:mem-ref (contacts i :geom :pos) :float 1))
          (setf (hit-position 2) (cffi:mem-ref (contacts i :geom :pos) :float 2))
          (setf (hit-position 3) (contacts i :geom :depth)))))))

(defmethod hit-p (raycast)
  (cffi-c-ref:c-let ((hit-position %ode:real :count 4 :from (hit raycast)))
    (%ode:space-collide2 (ray raycast) *space* hit-position (cffi:callback 'ray-callback))
    (hit-position 3)))
