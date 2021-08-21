(in-package #:scenic)

(defclass scene ()
  ((cameras      :initarg :cameras
                 :initform      (error ":cameras must be specified")
                 :accessor      cameras
                 :documentation "list of cameras")
   (lights       :initarg :lights
                 :initform      (error ":lights must be specified")
                 :reader        lights
                 :documentation "LIGHTS instance")
   (camera-index :initarg :camera-index
                 :reader  camera-index
                 :documentation "camera index of the active camera")
   (actors       :initarg :actors
                 :accessor      actors
                 :documentation "list of scene actors")
   (post         :initarg :post
                 :initform (error ":post must be specified")
                 :reader post
                 :documentation "post processing function")   )
  (:default-initargs
   :actors ()
   :camera-index 0)
  (:documentation "a single scene"))

(defmethod free ((obj scene))
  (free (lights obj))
  (mapc #'free (cameras obj))
  (mapc #'free (actors obj)))

(defun make-scene (cameras lights post)
  (make-instance 'scene :cameras cameras :lights lights :post post))

(defmethod update ((obj scene) dt)
  (dolist (c (cameras obj))
    (update c dt))
  (dolist (a (actors obj))
    (update a dt))
  (dolist (l (point-lights (lights obj)))
    (update l dt))
  (dolist (l (dir-lights (lights obj)))
    (update l dt)))

(defmethod draw :around ((obj scene) (camera renderable) time)
  (let ((fbo (fbo camera)))
    (with-fbo-bound (fbo)
      (clear-fbo fbo)
      (call-next-method))))

(defmethod draw ((obj scene) camera time)
  (dolist (a (actors obj))
    (draw a camera time)))

(defun current-scene ()
  (let ((state *state*))
    (nth (scene-index state) (scenes state))))

(defun current-camera ()
  (let ((scene (current-scene)))
    (nth (camera-index scene) (cameras scene))))

(defun current-lights ()
  (dir-lights (lights (current-scene))));; fixme: add poitnlights

(defun active-camera (scene)
  (nth (camera-index scene) (cameras scene)))

(defmethod upload ((obj scene))
  (dolist (m (materials *state*))
    (upload m))
  (upload (lights obj)))
