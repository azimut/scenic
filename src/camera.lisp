(in-package #:scenic)

(defclass camera ()
  ((pos  :initarg :pos  :accessor pos)
   (rot  :initarg :rot  :accessor rot)
   (near :initarg :near :accessor near)
   (far  :initarg :far  :accessor far)
   (fs   :initarg :fs   :accessor fs)
   (dim  :initarg :dim))
  (:default-initargs
   :pos (v! 0 0 0)
   :rot (q:identity)
   :near .1
   :far 400f0
   :fs nil))

(defclass renderable ()
  ((fbo :reader fbo :documentation "fbo to render to")
   (tex :reader tex :documentation "texture")
   (sam :reader sam :documentation "sample of the fbo texture")
   (texture-opts :initarg :texture-opts :documentation "texture options")
   (sample-opts  :initarg :sample-opts  :documentation "options for sample"))
  (:default-initargs
   :texture-opts '((0 (:dimensions (128 128) :element-type :r8))
                   (:d (:dimensions (128 128) :element-type :depth-component24)))
   :sample-opts '((:wrap :clap-to-border :minify-filter :nearest :magnify-filter :nearest)
                  (:wrap :clap-to-border :minify-filter :nearest :magnify-filter :nearest)))
  (:documentation "an fbo sampler pair"))

#+nil
((0 :dimensions (128 128) :element-type :layers 1)
 (1 :dimensions (128 128) :element-type :layers 1))

(defmethod initialize-instance :before ((obj renderable) &key texture-opts sample-opts)
  (assert (length= texture-opts sample-opts)))

(defun alloc-textures (texture-opts)
  "returns a list textures"
  (loop :for opt :in texture-opts
        :collect (apply #'make-texture nil (rest opt))))

(defun alloc-fbos (textures texture-opts)
  "returns 1 fbo"
  (let ((newopts (loop :for opt :in texture-opts
                       :for tex :in textures
                       :collect (list (first opt) tex))))
    (apply #'make-fbo newopts)))

(defun alloc-samplers (textures sample-opts)
  "returns a list of samples"
  (loop :for tex :in textures
        :for opt :in sample-opts
        :collect (apply #'sample tex sample-opts)))

(defmethod initialize-instance :after ((obj renderable) &key texture-opts sample-opts)
  (with-slots (fbo tex sam) obj
    (setf tex (alloc-textures texture-opts))
    (setf fbo (alloc-fbos tex texture-opts))
    (setf sam (alloc-samplers tex sample-opts))))

(defclass orth (camera renderable) ())
(defclass pers (camera renderable)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 60f0))

#+nil
(defmethod initialize-instance :after ((obj pers) &key)
  (with-slots (fbo sam dim) obj
    (setf fbo (make-fbo `(0  :dimensions ,dim :element-type :rgba32f)
                        `(:d :dimensions ,dim)))
    (setf sam (sample fbo))))

(defun make-orth (&rest args)
  (apply #'make-instance 'orth args))

(defun make-pers (&rest args)
  (apply #'make-instance 'pers args))

;; (defun distance-to-camera (pos distance)
;;   (< (v3:length (v3:- pos (pos *currentcamera*)))
;;      distance))

;; (defun next-camera ()
;;   "rotates current value on *CURRRENTCAMERA*
;;    for each on *CAMERAS*"
;;   (setf *cameras* (alexandria:rotate *cameras*))
;;   (setf *currentcamera* (first-elt *cameras*)))

(defun world->view (camera)
  (m4:* (q:to-mat4      (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defgeneric projection (camera)
  (:documentation "view to clip")
  (:method ((camera pers))
    (let ((fs (or (fs camera)
                  (viewport-resolution (current-viewport)))))
      (rtg-math.projection:perspective
       (x fs)
       (y fs)
       (near camera)
       (far camera)
       (fov camera))))
  (:method ((camera orth))
    (let ((fs (or (fs camera)
                  (viewport-resolution (current-viewport)))))
      (rtg-math.projection:orthographic
       (x fs)
       (y fs)
       (near camera)
       (far camera)))))

(defun world->clip (camera)
  (m4:* (projection camera)
        (world->view camera)))

(defun model->view (a c)
  (m4:* (world->view c) (model->world a)))

(defun model->clip (a c)
  (m4:* (world->clip c) (model->world a)))

;; Used for Raymarching. But I think it should be useful elsewhere.
;; https://github.com/Flafla2/Generic-Raymarch-Unity/blob/master/Assets/RaymarchGeneric.cs
(defmethod get-frustum-corners ((camera pers))
  "Stores the normalized rays representing the camera frustum in a 4x4 matrix.
  Each row is a vector.
  The following rays are stored in each row (in eyespace, not worldspace):
  Top Left corner:     row=0
  Top Right corner:    row=1
  Bottom Right corner: row=2
  Bottom Left corner:  row=3"
  (let* ((fov          (fov camera))
         ;; (aspect       (/ (first *dimensions*)
         ;;                  (second *dimensions*)))
         (aspect 1f0)
         (fov-half     (* fov .5))
         (tan-fov      (tan (radians fov-half)))
         (to-right     (v3:*s (v! 1 0 0) (* tan-fov aspect)))
         (to-top       (v3:*s (v! 0 1 0) tan-fov))
         (top-left     (v3:+ (v3:- (v! 0 0 -1) to-right) to-top))
         (top-right    (v3:+ (v3:+ (v! 0 0 -1) to-right) to-top))
         (bottom-right (v3:- (v3:+ (v! 0 0 -1) to-right) to-top))
         (bottom-left  (v3:- (v3:- (v! 0 0 -1) to-right) to-top)))
    (make-array 4 :initial-contents
                (list (v! top-left)
                      (v! top-right)
                      (v! bottom-right)
                      (v! bottom-left)))))

;; SetFrustumRays()
;; (defun get-frustum-rays-v3 ()
;;   (let ((camera *currentcamera*))
;;     (loop :for uv :in (list (v! 0 0) (v! 1 0) (v! 1 1) (v! 0 1))
;;           :collect (v3:- (m4:*v3 (m4:inverse (world->view camera))
;;                                  (v! uv (far camera)))
;;                          (pos camera)))))

;; (defun get-frustum-rays-v4 ()
;;   (let ((camera *currentcamera*))
;;     (loop :for uv :in (list (v! 0 0)
;;                             (v! 1 0)
;;                             (v! 1 1)
;;                             (v! 0 1))
;;           :collect (v! (v3:- (m4:*v3 (m4:inverse (world->view camera))
;;                                      (v! uv (far camera)))
;;                              (pos camera))
;;                        0f0))))


;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (camera dt))
(defmethod update ((camera orth) dt))
(defmethod update ((camera pers) dt))

