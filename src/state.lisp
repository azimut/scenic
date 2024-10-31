(in-package #:scenic)

(defvar *state* nil)

(defclass state (dirlights pointlights spotlights brdf screen)
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
    (set-and-upload-materials materials materials-ubo)))

(defmethod reinitialize-instance :after ((obj state) &key materials)
  (with-slots (scenes scene-index last-time materials-ubo) obj
    (set-and-upload-materials materials materials-ubo)
    (setf last-time (get-internal-real-time))
    (setf scene-index -1)
    (setf scenes ())))

(defun init-state (materials)
  (if *state*
      (reinitialize-instance *state* :materials materials)
      (setf *state* (make-instance 'state :materials materials))))

(defun init-default-state ()
  (init-state (list (make-material :roughness .8 :metallic .02 :specular .1)
                    (make-material :roughness .4 :metallic .4  :specular .1))))

(defmethod free ((obj state))
  (reset-material-counter)
  (mapc #'free (scenes obj)))

(defmethod (setf scene-index) :before (new-value (obj state))
  (check-type new-value alexandria:non-negative-integer)
  (assert (< new-value (length (scenes obj)))))

(defmethod (setf scene-index) :after (_ (obj state))
  (let ((scene (current-scene)))
    (dolist (light (lights scene))
      (setf (uploadp light) T
            (drawp   light) T))
    (let ((dim (dimensions (current-viewport))))
      (issue scene 'resize :width (nth 0 dim) :height (nth 1 dim)))))

(defun next-scene ()
  (let ((next (1+ (scene-index *state*)))
        (max  (length (scenes *state*))))
    (setf (scene-index *state*) (mod next max))
    (current-scene)))

(defmethod handle :around ((e resize) (obj perspective))
  (when (equal obj (current-camera))
    (call-next-method)))
