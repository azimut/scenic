(in-package #:scenic)

(defclass irradiance (capture)
  ((buf :initarg :buf :reader buf))
  (:default-initargs
   :buf (box)
   :dim '(32 32)
   :texture-opts '(( 0 :element-type :rgb16f :cubes t)
                   (:d :element-type :depth-component24 :cubes t))
   :sample-opts '((:wrap           :clamp-to-edge
                   :minify-filter  :linear
                   :magnify-filter :linear)
                  (:wrap           :clamp-to-edge
                   :minify-filter  :linear
                   :magnify-filter :linear)))
  (:documentation "doc"))

(defun make-irradiance (&rest args)
  (apply #'make-instance 'irradiance args))

(defun-g irradiance-vert ((vert g-pnt) &uniform (world :mat4))
  (* world (v! (pos vert) 1)))

(defun-g irradiance-geom (&uniform (projections shadow-projections :ubo))
  (declare (output-primitive :kind :triangle-strip :max-vertices 18))
  (dotimes (face 6)
    (setf gl-layer face)
    (dotimes (i 3)
      (let ((pos (gl-position (aref gl-in i))))
        (emit () (* (aref (shadow-projections-mats projections) face) pos)
              pos)))
    (end-primitive))
  (values))

(defun-g irradiance-frag ((frag-pos :vec4) &uniform (sam :sampler-cube))
  (let* ((pos    (s~ frag-pos :xyz))
         (up     (v! 0 1 0))
         (normal (normalize pos))
         (right  (cross up normal))
         (up     (cross normal right))
         (sample-delta 0.025)
         (nr-samples   0f0)
         (irradiance (v! 0 0 0)))
    (for
     (phi 0f0) (< phi (* 2 +pi+)) (setf phi (+ phi sample-delta))
     (for
      (theta 0f0) (< theta (* .5 +pi+)) (setf theta (+ theta sample-delta))
      (let* (;; spherical to cartesian (in tangent space)
             (tangent-sample (v! (* (sin theta) (cos phi))
                                 (* (sin theta) (sin phi))
                                 (cos theta)))
             ;; Tangent space to world
             (sample-vec (+ (* right  (x tangent-sample))
                            (* up     (y tangent-sample))
                            (* normal (z tangent-sample)))))
        (incf irradiance (* (s~ (texture sam sample-vec) :xyz)
                            (cos theta)
                            (sin theta)))
        (incf nr-samples 1f0))))
    (setf irradiance (* +pi+ irradiance (/ 1 nr-samples)))
    (v! irradiance 1)))

(defpipeline-g irradiance-pipe ()
  :vertex   (irradiance-vert g-pnt)
  :geometry (irradiance-geom)
  :fragment (irradiance-frag :vec4))

(defmethod draw ((scene scene-ibl) (camera irradiance) time)
  (with-setf* ((resolution (current-viewport)) (v! 32 32)
               (depth-test-function) #'<=
               (cull-face) :front)
    (with-slots (buf ubo) camera
      (map-g #'irradiance-pipe buf
             :projections ubo
             :world (model->world camera)
             :sam (first (sam (capture scene)))))))
