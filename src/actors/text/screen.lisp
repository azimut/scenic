(in-package #:scenic)

(defclass screen (font postprocess)
  ((pos   :reader pos   :initarg :pos)
   (color :reader color :initarg :color))
  (:default-initargs
   :pos (v! 0 0)
   :color (v! 1 1 1 1))
  (:documentation "2D on screen text"))

(defmethod print-object ((obj screen) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos path) obj
      (format stream "(~$ ~$) ~s" (x pos) (y pos) path))))

(defun make-screen (&rest args)
  (apply #'make-instance 'screen args))

(defmethod initialize-instance :after ((obj screen) &key msg)
  (setf (msg obj) msg))

(defmethod blit (scene (obj screen) camera time)
  (with-slots (pos color fond-text) obj
    (cepl.fond:fond-draw-simple
     fond-text
     (v! pos (dim camera))
     color)))

