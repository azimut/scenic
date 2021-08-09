(in-package #:scenic)

(defvar *state* nil)

(defclass state ()
  ((scenes      :initarg :scenes
                :initform (error ":scenes must be specified")
                :accessor scenes)
   (scene-index :initarg :scene-index
                :initform 0
                :reader   scene-index
                :documentation "current scene index"))
  (:documentation "main state"))

(defun init-state (scenes)
  (setf *state* (make-instance 'state :scenes scenes)))

(defclass scene ()
  ((cameras      :initarg :cameras
                 :initform      (error ":cameras must be specified")
                 :accessor      cameras
                 :documentation "list of cameras")
   (lights       :initarg :lights
                 :initform      (error ":lights must be specified")
                 :reader        lights
                 :documentation "list of lights")
   (camera-index :initarg :camera-index
                 :reader  camera-index
                 :documentation "camera index of the active camera")
   (actors       :initarg :actors
                 :accessor      actors
                 :documentation "list of scene actors"))
  (:default-initargs
   :actors ()
   :camera-index 0)
  (:documentation "a single scene"))

(defun make-scene (cameras lights)
  (make-instance 'scene :cameras cameras :lights lights))

(defun current-scene ()
  (let ((state *state*))
    (nth (scene-index state) (scenes state))))

(defun current-camera ()
  (let ((scene (current-scene)))
    (nth (camera-index scene) (cameras scene))))

(defun current-lights ()
  (lights (current-scene)))
