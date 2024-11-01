(in-package #:scenic)

;; Reference:
;; https://learnopengl.com/Advanced-Lighting/Bloom
;; https://catlikecoding.com/unity/tutorials/advanced-rendering/bloom/

(defclass bloom (postprocess)
  ((blending        :reader   blending        :initarg :blending)
   (intensity       :accessor intensity       :initarg :intensity) ;; TODO
   (intensity-gamma :reader   intensity-gamma)
   (threshold       :accessor threshold       :initarg :threshold)
   (threshold-soft  :accessor threshold-soft  :initarg :threshold-soft)
   (fbos            :reader   fbos))
  (:default-initargs
   :intensity      1f0 ;; 1.0 - 10.0
   :threshold      1f0
   :threshold-soft 0.5
   :blending (make-blending-params :source-rgb :one :destination-rgb :one)))

;; https://github.com/DukeChiang/DCET/blob/52cde71d239e6023daf42a0d84f04041c235e413/Server/Packages/UnityEngine/Unity/Mathf.cs#L681
(defun gamma-to-linear-space (n)
  "From Unity's Mathf.GammaToLinearSpace"
  (cond ((<= n 0.04045) (/ n 12.92))
        ((<  n 1f0)     (expt (/ (+ n 0.055) 1.055) 2.4))
        (t              (expt n 2.4))))

(defmethod initialize-instance :after ((obj bloom) &key intensity)
  (setf (slot-value obj 'fbos) (make-bloom-fbo))
  (setf (slot-value obj 'intensity-gamma) (gamma-to-linear-space intensity)))

(defmethod (setf intensity) :after (new-value (obj bloom))
  (setf (slot-value obj 'intensity-gamma) (gamma-to-linear-space new-value)))

(defstruct (bloom-fbo (:constructor %make-bloom-fbo))
  (fbos     (make-array 5))
  (samplers (make-array 5))
  (widths   (make-array 5 :element-type 'single-float))
  (heights  (make-array 5 :element-type 'single-float)))

(defmethod free ((obj bloom))
  (map NIL #'free (bloom-fbo-fbos (fbos obj))))

(defun make-bloom-fbo ()
  (flet ((f (d) (mapcar (lambda (x) (floor (/ x d)))
                        (viewport-dimensions (current-viewport)))))
    (let ((obj (%make-bloom-fbo)))
      (loop :for div    :in '(1 2 4 8 16)
            :for width  := (nth 0 (f div))
            :for height := (nth 1 (f div))
            :for i :from 0
            :do (let* ((fbo (make-fbo `(0 :dimensions ,(f div) :element-type :rgba16f)))
                       (sam (sample (attachment-tex fbo 0) :wrap :clamp-to-edge)))
                  (setf (aref (bloom-fbo-widths   obj) i) (/ (coerce width  'single-float)))
                  (setf (aref (bloom-fbo-heights  obj) i) (/ (coerce height 'single-float)))
                  (setf (aref (bloom-fbo-fbos     obj) i) fbo)
                  (setf (aref (bloom-fbo-samplers obj) i) sam)))
      obj)))

(defmethod handle ((e resize) (obj bloom))
  (let ((oldfbos (fbos obj))
        (newfbos (make-bloom-fbo)))
    (setf (slot-value obj 'fbos) newfbos)
    (free oldfbos)))
