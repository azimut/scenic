(in-package #:scenic)

(defclass capture (pers renderable)
  ((ubo     :reader   ubo)
   (uploadp :accessor uploadp :initarg :uploadp)
   (drawp   :accessor drawp   :initarg :drawp))
  (:default-initargs
   :texture-opts '(( 0 :element-type :rgb16f :cubes t)
                   (:d :element-type :depth-component24 :cubes t))
   :sample-opts '((:wrap :clamp-to-edge :minify-filter :linear :magnify-filter :linear)
                  (:wrap :clamp-to-edge :minify-filter :linear :magnify-filter :linear))
   :dim '(128 128)
   :uploadp T
   :drawp T
   :fov 90f0
   :fs (v! 1 1)
   :near .01f0
   :far 100f0)
  (:documentation "cubemap capture of the current scene"))

(defmethod print-object ((obj capture) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos drawp uploadp dim) obj
      (format stream "pos:(~a ~a ~a) dim:(~a ~a) d?:~a u?:~a"
              (x pos) (y pos) (z pos) (first dim) (second dim) drawp uploadp))))

(defmethod (setf near) :after (_ (obj capture)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf pos)  :after (_ (obj capture)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf far)  :after (_ (obj capture)) (setf (uploadp obj) T (drawp obj) T))

(defun make-capture (&rest args)
  (apply #'make-instance 'capture args))

(defmethod initialize-instance :after ((obj capture) &key)
  (with-slots (ubo) obj
    (setf ubo (make-ubo NIL 'shadow-projections))
    (upload obj)))

(defmethod free ((obj capture))
  (free (ubo obj)))

(defmethod upload ((obj capture))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (shadow-projections-mats (aref-c c 0)) (projection-mats obj))))

(defun-g capture-vert ((vert g-pnt) &uniform (world :mat4))
  (values (* world (v! (pos vert) 1))
          (* (m4:to-mat3 world) (norm vert))))

(defun-g capture-geom ((frag-norm (:vec3 3)) &uniform (projections shadow-projections :ubo))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 18))
  (dotimes (face 6)
    (setf gl-layer face)
    (dotimes (i 3)
      (let ((pos (gl-position (aref gl-in i))))
        (emit ()
              (* (aref (shadow-projections-mats projections) face)
                 pos)
              pos
              (aref frag-norm i))))
    (end-primitive))
  (values))

(defun-g capture-frag ((frag-pos :vec4) (frag-norm :vec3) &uniform
                       (color       :vec3)
                       (scene       scene-data       :ubo)
                       (dirlights   dir-light-data   :ubo)
                       (spotlights  spot-light-data  :ubo)
                       (pointlights point-light-data :ubo))
  (let ((final-color (v! 0 0 0)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions) dirlights; TODO: SHADOW and light in clipspace?
        (incf final-color (dir-light-apply color (aref colors i) (aref positions i)
                                           (s~ frag-pos :xyz) frag-norm))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic) pointlights
        (incf final-color (point-light-apply color (aref colors i) (aref positions i)
                                             (s~ frag-pos :xyz) frag-norm
                                             (aref linear i) (aref quadratic i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction) spotlights
        (incf final-color (spot-light-apply color (aref colors i) (aref positions i) (aref direction i)
                                            (s~ frag-pos :xyz) frag-norm
                                            (aref linear i) (aref quadratic i)
                                            (aref cutoff i) (aref outer-cutoff i)))))
    (v! final-color 1)
    (v! 0 1 0 1)))

(defpipeline-g capture-pipe ()
  :vertex   (capture-vert g-pnt)
  :geometry (capture-geom (:vec3 3))
  :fragment (capture-frag :vec4 :vec3))

(defmethod draw :around ((scene scene) (camera capture) time)
  (when (drawp camera)
    (let ((fbo (fbo camera)))
      (with-fbo-bound (fbo)
        (clear-fbo fbo)
        (call-next-method)))
    (setf (drawp camera) NIL)))

(defmethod draw ((scene scene) (camera capture) time)
  (dolist (a (actors scene))
    (draw a camera time)))

;; NOTE: End of the Road with :around, to avoid other more specific draw calls
(defmethod draw :around ((actor actor) (camera capture) time)
  (with-slots (buf color) actor
    (map-g #'capture-pipe buf
           :color       color
           :world       (model->world actor)
           :scene       (ubo (current-scene))
           :dirlights   (dir-ubo *state*)
           :spotlights  (spot-ubo *state*)
           :pointlights (point-ubo *state*)
           :projections (ubo camera))))
