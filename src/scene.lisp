(in-package #:scenic)

(defclass scene (event-loop)
  ((cameras      :initarg :cameras
                 :initform ()
                 :accessor      cameras
                 :documentation "list of cameras")
   (camera-index :initarg :camera-index
                 :reader  camera-index
                 :documentation "camera index of the active camera")
   (lights       :initarg :lights
                 :initform ()
                 :accessor        lights
                 :documentation "LIGHTS instance")
   (actors       :initarg :actors
                 :accessor      actors
                 :documentation "list of scene actors")
   (post         :initarg :post
                 :accessor post
                 :documentation "post processing function")
   (ubo          :reader ubo :documentation "scene-data UBO")
   (color        :initarg :color
                 :accessor color
                 :documentation "(clear-color) color")
   (name         :reader name :initarg :name))
  (:default-initargs
   :post (list (make-simple-postprocess))
   :actors ()
   :color (v! 0 0 0 0)
   :camera-index 0)
  (:documentation "a single scene"))

(defmethod print-object ((obj scene) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (actors lights cameras) obj
      (if (slot-boundp obj 'name)
          (format stream "~s A:~a L:~a C:~a"
                  (slot-value obj 'name)
                  (length actors) (length lights) (length cameras))
          (format stream "A:~a L:~a C:~a"
                  (length actors) (length lights) (length cameras))))))

(defstruct-g (scene-data :layout :std-140)
  (ndir   :int)
  (nspot  :int)
  (npoint :int))

(defun make-scene (&rest args)
  (apply #'make-instance 'scene args))

#+nil
(defun init-collection (lights)
  (let ((idx 0))
    (dolist (light lights)
      (init-light light idx)
      (incf idx))))

(defmethod (setf color) (new-color (scene scene))
  (etypecase new-color
    (rtg-math.types:vec3
     (setf (slot-value scene 'color)
           (v! (x new-color) (y new-color) (z new-color) 1)))
    (rtg-math.types:vec4
     (setf (slot-value scene 'color) new-color))))

(defmethod initialize-instance :before ((obj scene) &key color)
  (check-type color rtg-math.types:vec4))

(defmethod initialize-instance :after ((obj scene) &key post)
  (mapcar (lambda (p) (add-listener p obj)) post )
  (with-slots (ubo) obj
    (setf ubo (make-ubo NIL 'scene-data))
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (scene-data-ndir   (aref-c c 0)) 0)
      (setf (scene-data-nspot  (aref-c c 0)) 0)
      (setf (scene-data-npoint (aref-c c 0)) 0))))

(defmethod free ((obj scene))
  (mapc #'free (cameras obj))
  (mapc #'free (actors obj)))

(defmethod upload ((scene scene))
  (dolist (m (materials *state*))
    (upload m))
  (dolist (l (lights scene))
    (upload l)))

(defmethod update ((obj scene) dt)
  (dolist (c (cameras obj))
    (update c dt))
  (dolist (a (actors obj))
    (update a dt))
  (dolist (l (lights obj))
    (update l dt)))

(defgeneric paint (scene actor camera time)
  (:documentation "final stage of drawing for an individual ACTOR"))

(defun current-scene ()
  (let ((state *state*))
    (nth (scene-index state) (scenes state))))

(defun current-camera ()
  (let ((scene (current-scene)))
    (nth (camera-index scene) (cameras scene))))

(defun active-camera (scene)
  (nth (camera-index scene) (cameras scene)))

(defmethod draw ((scene scene) (camera renderable) time)
  (dolist (l (lights scene))
    (paint scene l camera time))
  (dolist (a (actors scene))
    (paint scene a camera time)))

(defmethod draw :around ((obj scene) (camera renderable) time)
  (let ((fbo (fbo camera)))
    (with-fbo-bound (fbo)
      (with-setf (clear-color) (color obj)
        (clear-fbo fbo)
        (call-next-method)))))
