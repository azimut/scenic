(in-package #:scenic)

(defclass raycast (uploadable)
  ((from :accessor from :initarg :from)
   (to   :accessor to   :initarg :to)
   (ray  :reader   ray)
   (hit  :reader   hit)
   (gar  :reader   gar))
  (:documentation "ode raycast"))

(defun make-raycast (&rest args)
  (apply #'make-instance 'raycast args))

(defmethod print-object ((obj raycast) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (from to) obj
      (format stream "(~,2f ~,2f ~,2f)->(~,2f ~,2f ~,2f)"
              (x from) (y from) (z from) (x to) (y to) (z to)))))

(defmethod initialize-instance :before ((obj raycast) &key from to)
  (assert (not (v3:= from to))))
(defmethod initialize-instance :after ((obj raycast) &key)
  (setf (slot-value obj 'hit) (cffi:foreign-alloc '%ode:real :count 4))
  (setf (slot-value obj 'ray) (%ode:create-ray *space* 0f0))
  (setf (slot-value obj 'gar) (make-gpu-array nil :element-type :vec3 :dimensions 2))
  (upload obj))

(defmethod upload ((obj raycast))
  (with-slots (from to ray gar) obj
    (let ((dir (v3:normalize (v3:- to from))))
      (%ode:geom-ray-set ray (x from) (y from) (z from) (x dir) (y dir) (z dir))
      (%ode:geom-ray-set-length ray (v3:distance to from)))
    (with-gpu-array-as-c-array (carr gar)
      (setf (aref-c carr 0) (v! (x from) (y from) (z from)))
      (setf (aref-c carr 1) (v! (x to) (y to) (z to))))))

(defmethod (setf from) :after (_ (obj raycast)) (setf (uploadp obj) T))
(defmethod (setf to)   :after (_ (obj raycast)) (setf (uploadp obj) T))

(defmethod free ((obj raycast))
  (%ode:geom-destroy (ray obj))
  (cffi:foreign-free (hit obj))
  (free (gar obj)))

(cffi:defcallback ray-callback
    :void ((data :pointer) (g1 %ode:geom-id) (g2 %ode:geom-id))
  (cffi-c-ref:c-let ((hit-position %ode:real :count 4 :from data))
    (cffi-c-ref:c-with ((contacts %ode:contact :count 3))
      (dotimes (i (%ode:collide g1 g2 3 (contacts :geom &) (cffi:foreign-type-size '%ode:contact)))
        (when (< (contacts i :geom :depth)
                 (hit-position 3))
          (setf (hit-position 0) (cffi:mem-ref (contacts i :geom :pos) :float 0))
          (setf (hit-position 1) (cffi:mem-ref (contacts i :geom :pos) :float 1))
          (setf (hit-position 2) (cffi:mem-ref (contacts i :geom :pos) :float 2))
          (setf (hit-position 3) (contacts i :geom :depth)))))))

(defmethod hit-p (raycast)
  (cffi-c-ref:c-let ((hit-position %ode:real :count 4 :from (hit raycast)))
    (setf (hit-position 3) most-positive-single-float)
    (%ode:space-collide2 (ray raycast) *space* (hit-position &) (cffi:callback ray-callback))
    (v! (hit-position 0) (hit-position 1) (hit-position 2) (hit-position 3))))

(defun-g line-vert ((vert :vec3) &uniform (model-clip :mat4))
  (* model-clip (v! vert 1)))
(defun-g line-frag (&uniform (color :vec3))
  (v! color 1))
(defpipeline-g line-pipe (:lines)
  (line-vert :vec3)
  (line-frag))

(defmethod paint (scene (obj raycast) camera time)
  (with-slots (buf color) obj
    (map-g #'line-pipe buf
           :model-clip (model->clip obj camera)
           :color color)))
