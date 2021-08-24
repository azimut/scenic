(in-package #:scenic)

(defvar *state* nil)

(defclass state (dirlights pointlights spotlights)
  ((scenes        :initarg :scenes
                  :initform ()
                  :accessor scenes)
   (scene-index   :accessor   scene-index
                  :initform 0
                  :documentation "current scene index")
   (materials     :initarg :materials
                  :accessor materials
                  :initform (error ":materials must be specified")
                  :documentation "list of materials")
   (materials-ubo :reader   materials-ubo
                  :documentation "ubo of materials")
   (last-time     :initarg :last-time
                  :accessor last-time
                  :documentation "previous loop time"))
  (:default-initargs
   :last-time (get-internal-real-time))
  (:documentation "main state"))

(defmethod initialize-instance :after ((obj state) &key materials)
  (with-slots (materials-ubo) obj
    (setf materials-ubo (make-ubo NIL 'pbr-material))
    (mapc (lambda (material)
            (setf (slot-value material 'ubo) materials-ubo)
            (upload material))
          materials)))

(defun init-state (materials)
  (setf *state* (make-instance 'state :materials materials)))

(defmethod free ((obj state))
  (free (slot-value obj 'materials-ubo))
  (mapc #'free (scenes obj)))

(defmethod (setf scene-index) :before (new-value (obj state))
  (check-type new-value alexandria:non-negative-integer)
  (assert (< new-value (length (scenes obj)))))

(defmethod (setf scene-index) :after (_ (obj state))
  (let ((scene (current-scene)))
    (dolist (light (lights scene))
      (setf (uploadp light) T
            (drawp   light) T))))

(defun next-scene ()
  (let ((next (1+ (scene-index *state*)))
        (max  (length (scenes *state*))))
    (setf (scene-index *state*) (mod next max))))
