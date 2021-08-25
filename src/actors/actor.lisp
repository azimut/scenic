(in-package #:scenic)

(defclass actor ()
  ((pos      :initarg :pos      :accessor pos      :documentation "3d position")
   (rot      :initarg :rot      :accessor rot      :documentation "3d rotation")
   (buf      :initarg :buf      :accessor buf      :documentation "buffer stream")
   (color    :initarg :color    :accessor color    :documentation "base color")
   (scale    :initarg :scale    :accessor scale    :documentation "vextex fudge scale")
   (material :initarg :material :accessor material :documentation "material index")
   (shadowp  :initarg :shadowp  :accessor shadowp  :documentation "if it casts shadows or not"))
  (:default-initargs
   :shadowp T
   :material 0
   :color (v! 1 1 1)
   :pos (v! 0 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box 1 1 1 t))
  (:documentation "base object, with tangents"))

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos) obj
      (format stream "(~a ~a ~a)" (x pos) (y pos) (z pos)))))

(defmethod update ((obj actor) dt))
