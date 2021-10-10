(in-package #:scenic)

(defclass base-particles (listener)
  ((n-particles :initarg :n-particles)
   (gar-src     :initarg :gar-src)
   (gar-dst     :initarg :gar-dst)
   (str-src     :initarg :str-src)
   (str-dst     :initarg :str-dst)
   (tfs-src     :initarg :tfs-src)
   (tfs-dst     :initarg :tfs-dst)
   (bs          :reader bs :allocation :class))
  (:default-initargs
   :n-particles 100)
  (:documentation "template class for particles"))

(defstruct-g pdata
  (pos  :vec3)
  (dir  :vec3)
  (life :float))

(defun-g pinit-vert ()
  (values (v! 0 0 0 0)
          (:feedback (v! 0 0 0))
          (:feedback (v! 0 0 0))
          (:feedback 0f0)))

(defpipeline-g pinit-pipe (:points)
  :vertex (pinit-vert))

(defun init-particles (actor)
  (with-slots (tfs-src tfs-dst bs) actor
    (with-transform-feedback (tfs-src)
      (map-g #'pinit-pipe bs))
    (with-transform-feedback (tfs-dst)
      (map-g #'pinit-pipe bs)))
  (values))

(defmethod initialize-instance :after ((obj base-particles) &key n-particles)
  (with-slots (gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (setf gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          str-src (make-buffer-stream gar-src :primitive :points)
          str-dst (make-buffer-stream gar-dst :primitive :points)
          tfs-src (make-transform-feedback-stream gar-src)
          tfs-dst (make-transform-feedback-stream gar-dst)))
  (unless (slot-boundp obj 'bs)
    (setf (slot-value obj 'bs) (make-buffer-stream nil :primitive :points)))
  (init-particles obj))

(defmethod free :after ((obj base-particles))
  (with-slots (gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (free str-src)
    (free str-dst)
    (free gar-src)
    (free gar-dst)
    (setf tfs-src nil)
    (setf tfs-dst nil)))
