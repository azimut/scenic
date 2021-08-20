(in-package #:scenic)

(defvar *state* nil)

(defclass state ()
  ((scenes        :initarg :scenes
                  :initform (error ":scenes must be specified")
                  :accessor scenes)
   (scene-index   :initarg :scene-index
                  :initform 0
                  :reader   scene-index
                  :documentation "current scene index")
   (materials     :initarg :materials
                  :initform (error ":materials must be specified")
                  :reader materials
                  :documentation "list of materials")
   (materials-ubo :initarg :materials-ubo
                  :reader materials-ubo
                  :documentation "ubo of materials"))
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
