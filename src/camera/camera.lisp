(in-package #:scenic)

(defclass camera (listener)
  ((pos  :initarg :pos  :accessor pos)
   (rot  :initarg :rot  :accessor rot)
   (near :initarg :near :accessor near)
   (far  :initarg :far  :accessor far)
   (fs   :initarg :fs   :accessor fs))
  (:default-initargs
   :pos (v! 0 0 0)
   :rot (q:identity)
   :near .1
   :far 400f0
   :fs nil))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 60f0))
(defun make-orth (&rest args) (apply #'make-instance 'orth args))
(defun make-pers (&rest args) (apply #'make-instance 'pers args))

(defclass orthogonal (renderable orth) ())
(defclass perspective (renderable pers) ())
(defun make-orthogonal (&rest args) (apply #'make-instance 'orthogonal args))
(defun make-perspective (&rest args) (apply #'make-instance 'perspective args))

(defmethod handle ((e resize) (obj perspective))
  ;;(resize obj (width e) (height e))
  (setf (resolution (current-viewport)) (v! (first (dim obj)) (second (dim obj)))))


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
(defmethod update ((camera orthogonal) dt))
(defmethod update ((camera perspective) dt))
