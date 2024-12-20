(in-package #:scenic)

(defclass material ()
  ((fakeambient :accessor fakeambient :initarg :fakeambient
                :documentation "used in forward rendering, to fudge a fake ambient lighting")
   (roughness   :accessor roughness   :initarg :roughness)
   (specular    :accessor specular    :initarg :specular)
   (metallic    :accessor metallic    :initarg :metallic)
   (emissive    :accessor emissive    :initarg :emissive)
   (aocclusion  :accessor aocclusion  :initarg :aocclusion)
   (uploadp     :accessor uploadp     :initarg :uploadp)
   (ubo         :reader   ubo         :documentation "reserved for a reference to the scene ubo")
   (idx         :reader   idx))
  (:default-initargs
   :fakeambient 0.0
   :aocclusion  1.0
   :roughness   0.7
   :specular    0.2
   :metallic    0.04
   :emissive    0.0
   :uploadp     T)
  (:metaclass counted-class)
  (:documentation "pbr material"))

(defun make-material (&rest args)
  (apply #'make-instance 'material args))

(defmethod print-object ((obj material) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (specular metallic emissive roughness fakeambient aocclusion idx) obj
      (format stream "IDX:~a R:~a E:~a M:~a S:~a FAO:~a AO:~a"
              idx roughness emissive metallic specular fakeambient aocclusion))))

(defmethod (setf specular)    :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf metallic)    :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf emissive)    :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf roughness)   :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf fakeambient) :after (_ (obj material)) (setf (uploadp obj) T))
(defmethod (setf aocclusion)  :after (_ (obj material)) (setf (uploadp obj) T))


(defun reset-material-counter ()
  (setf (slot-value (find-class 'material) 'counter) 0))
(defun current-material-counter ()
  (slot-value (find-class 'material) 'counter))

;; NOTE: can't upload here, as the UBO is not allocated yet.
(defmethod initialize-instance :after ((obj material) &key)
  (setf (slot-value obj 'idx) (current-material-counter)))

(defstruct-g (pbr-material :layout :std-140)
  (specular    (:float 5))
  (metallic    (:float 5))
  (emissive    (:float 5))
  (roughness   (:float 5))
  (fakeambient (:float 5))
  (aocclusion  (:float 5)))

(defmethod upload ((obj material))
  (with-slots (ubo idx specular metallic emissive roughness fakeambient aocclusion)
      obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (aref-c (pbr-material-fakeambient  (aref-c c 0)) idx) fakeambient)
      (setf (aref-c (pbr-material-roughness    (aref-c c 0)) idx) roughness)
      (setf (aref-c (pbr-material-specular     (aref-c c 0)) idx) specular)
      (setf (aref-c (pbr-material-metallic     (aref-c c 0)) idx) metallic)
      (setf (aref-c (pbr-material-emissive     (aref-c c 0)) idx) emissive)
      (setf (aref-c (pbr-material-aocclusion   (aref-c c 0)) idx) aocclusion))))

(defun set-and-upload-materials (materials materials-ubo)
  (mapc (lambda (m)
          (setf (slot-value m 'ubo) materials-ubo)
          (upload m))
        materials))
