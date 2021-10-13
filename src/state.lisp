(in-package #:scenic)

(defvar *state* nil)

(defclass state (dirlights pointlights spotlights brdf)
  ((scenes        :initarg :scenes        :accessor scenes        :documentation "list of scenes")
   (scene-index   :initarg :scene-index   :accessor scene-index   :documentation "current scene index")
   (last-time     :initarg :last-time     :accessor last-time     :documentation "previous loop time")
   (materials     :initarg :materials     :accessor materials     :documentation "list of materials")
   (materials-ubo :initarg :materials-ubo :reader   materials-ubo :documentation "ubo of materials"))
  (:default-initargs
   :scenes ()
   :scene-index -1
   :materials (error ":materials must be specified")
   :last-time (get-internal-real-time))
  (:documentation "main state"))

(defmethod initialize-instance :after ((obj state) &key materials)
  (with-slots (materials-ubo) obj
    (setf materials-ubo (make-ubo NIL 'pbr-material))
    (mapc (lambda (m)
            (setf (slot-value m 'ubo) materials-ubo)
            (upload m))
          materials)))

(defun init-state (materials)
  (setf *state* (make-instance 'state :materials materials)))

(defmethod free ((obj state))
  (free (materials-ubo obj))
  (mapc #'free (scenes obj)))

(defmethod (setf scene-index) :before (new-value (obj state))
  (check-type new-value alexandria:non-negative-integer)
  (assert (< new-value (length (scenes obj)))))

(defmethod (setf scene-index) :after (_ (obj state))
  (let ((scene (current-scene)))
    (dolist (light (lights scene))
      (setf (uploadp light) T
            (drawp   light) T)))
  (let ((dim (dimensions (current-viewport))))
    (issue obj 'resize :width (nth 0 dim) :height (nth 1 dim))))

(defun next-scene ()
  (let ((next (1+ (scene-index *state*)))
        (max  (length (scenes *state*))))
    (setf (scene-index *state*) (mod next max))
    (current-scene)))
