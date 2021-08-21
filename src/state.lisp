(in-package #:scenic)

(defvar *state* nil)

(defclass state ()
  ((scenes        :initarg :scenes
                  :accessor scenes
                  :initform (error ":scenes must be specified"))
   (scene-index   :reader   scene-index
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

(defun init-state (materials scenes)
  (setf *state* (make-instance 'state :materials materials :scenes scenes)))

(defmethod free ((obj state))
  (free (slot-value obj 'materials-ubo))
  (mapc #'free (scenes obj)))
