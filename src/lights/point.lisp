(in-package #:scenic)

(defclass point (pers light)
  ((linear    :initarg :linear    :accessor linear    :documentation "linear factor of pointlight decay")
   (quadratic :initarg :quadratic :accessor quadratic :documentation "quadratic factor of pointlight decay"))
  (:default-initargs
   :fov 90f0
   :fs (v! 1 1)
   :near 0.001
   :far 50f0
   :linear 0.14
   :quadratic 0.07)
  (:documentation "simple pointlight light"))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "POS: ~a LIN: ~a QUA: ~a NEAR: ~a FAR: ~a"
            (slot-value obj 'pos)
            (slot-value obj 'linear) (slot-value obj 'quadratic)
            (slot-value obj 'near)   (slot-value obj 'far))))

(defmethod (setf linear) :after (new-val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-linear (aref-c c 0)) (idx obj)) new-val)))
(defmethod (setf quadratic) :after (new-val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-quadratic (aref-c c 0)) (idx obj)) new-val)))

(defun projection-mats (light)
  "Returns a list of 6 m4 projection matrices"
  (let ((projection (projection light))
        (light-pos  (pos light)))
    (list
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  1  0  0))))
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v! -1  0  0))))
     (m4:* projection (m4:look-at (v! 0  0  1) light-pos (v3:+ light-pos (v!  0  1  0))))
     (m4:* projection (m4:look-at (v! 0  0 -1) light-pos (v3:+ light-pos (v!  0 -1  0))))
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  0  0  1))))
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  0  0 -1)))))))

(defun upload-projection (light)
  (with-gpu-array-as-c-array (c (ubo-data (ubo light)))
    (setf (aref-c (shadowspace (aref-c c 0)) (idx light))
          (projection-mats light))))

(defmethod (setf pos)  :after (_ (obj point))
  (upload-transform obj))
(defmethod (setf far)  :after (_ (obj point))
  (upload-projection obj)
  (upload-transform obj))
(defmethod (setf near) :after (_ (obj point))
  (upload-projection obj)
  (upload-transform obj))

(defmethod init-light :after ((obj directional) idx ubo tex)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx))))
  (upload-transform obj)
  (upload-projection obj))

(defun make-point (&rest args)
  (apply #'make-instance 'point args))

