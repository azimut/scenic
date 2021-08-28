(in-package #:scenic)

(defclass scene ()
  ((cameras      :initarg :cameras
                 :initform      (error ":cameras must be specified")
                 :accessor      cameras
                 :documentation "list of cameras")
   (camera-index :initarg :camera-index
                 :reader  camera-index
                 :documentation "camera index of the active camera")
   (lights       :initarg :lights
                 :initform      (error ":lights must be specified")
                 :reader        lights
                 :documentation "LIGHTS instance")
   (actors       :initarg :actors
                 :accessor      actors
                 :documentation "list of scene actors")
   (post         :initarg :post
                 :initform (error ":post must be specified")
                 :reader post
                 :documentation "post processing function")
   (ubo          :reader ubo :documentation "scene-data UBO")
   (color        :initarg :color
                 :accessor color
                 :documentation "(clear-color) color"))
  (:default-initargs
   :actors ()
   :color (v! 0 0 0 0)
   :camera-index 0)
  (:documentation "a single scene"))

(defstruct-g (scene-data :layout :std-140)
  (ndir   :int)
  (nspot  :int)
  (npoint :int))

(defun make-scene (cameras lights post &key (color (v! 0 0 0 0)))
  (make-instance 'scene :cameras cameras :lights lights :post post :color color))

(defun init-collection (lights)
  (let ((idx 0))
    (dolist (light lights)
      (init-light light idx)
      (incf idx))))

(defmethod (setf color) :before (new-value (obj scene))
  (check-type new-value rtg-math.types:vec4))

(defmethod initialize-instance :before ((obj scene) &key color)
  (check-type color rtg-math.types:vec4))

(defmethod initialize-instance :after ((obj scene) &key lights)
  (let ((ndir 0) (nspot 0) (npoint 0)
        (dirs ()) (points ()) (spots ()))
    (dolist (light lights)
      (etypecase light
        (directional (push light dirs)   (incf ndir))
        (point       (push light points) (incf npoint))
        (spot        (push light spots)  (incf nspot))))
    (with-slots (ubo) obj
      (setf ubo (make-ubo NIL 'scene-data))
      (with-gpu-array-as-c-array (c (ubo-data ubo))
        (setf (scene-data-ndir   (aref-c c 0)) ndir)
        (setf (scene-data-nspot  (aref-c c 0)) nspot)
        (setf (scene-data-npoint (aref-c c 0)) npoint)))
    (init-collection dirs)
    (init-collection spots)
    (init-collection points)))

(defmethod free ((obj scene))
  (mapc #'free (cameras obj))
  (mapc #'free (actors obj)))

(defmethod update ((obj scene) dt)
  (dolist (c (cameras obj))
    (update c dt))
  (dolist (a (actors obj))
    (update a dt))
  (dolist (l (lights obj))
    (update l dt)))

(defmethod draw :around ((obj scene) (camera renderable) time)
  (let ((fbo (fbo camera)))
    (with-fbo-bound (fbo)
      (with-setf (clear-color) (color obj)
        (clear-fbo fbo)
        (call-next-method)))))

(defmethod draw ((obj scene) camera time)
  (dolist (a (actors obj))
    (draw a camera time)))

(defun current-scene ()
  (let ((state *state*))
    (nth (scene-index state) (scenes state))))

(defun current-camera ()
  (let ((scene (current-scene)))
    (nth (camera-index scene) (cameras scene))))

(defun active-camera (scene)
  (nth (camera-index scene) (cameras scene)))

(defmethod upload ((obj scene))
  (dolist (m (materials *state*))
    (upload m))
  (dolist (l (lights obj))
    (upload l)))
