(in-package #:scenic)

(defclass defered (renderable pers)
  ((fakeambient :initarg :fakeambient
                :accessor fakeambient
                :documentation "used to calculate a fake ambient light, on defered a single value works well enough for all materials")
   (blending    :initform (make-blending-params)
                :reader blend
                :documentation "used to blend environment maps on blit"))
  (:default-initargs
   :fakeambient 0f0
   :texture-opts
   '((0  :element-type :rgba32f); color    roughness (32 due I am abusing this as a hdr render for skybox)
     (1  :element-type :rgba32f); pos      ao
     (2  :element-type :rgba16f); norm     specular
     (3  :element-type :rg16f)  ; metallic emissive
     (:d :element-type :depth-component24))
   :sample-opts
   '((:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)))
  (:documentation "defered perspective renderable camera"))

(defmethod initialize-instance :before ((obj defered) &key fakeambient)
  (check-type fakeambient (float 0 1)))
(defmethod (setf fakeambient) :before (new-value (obj defered))
  (check-type new-value (float 0 1)))

(defun make-defered (&rest args)
  (apply #'make-instance 'defered args))

(defmethod handle :around ((e resize) (obj defered))
  (when (equal obj (current-camera))
    (call-next-method)))
(defmethod handle ((e resize) (obj defered))
  (setf (dim obj) (list (width e) (height e))))
