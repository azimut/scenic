(in-package #:scenic)

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

;; NOTE: untextured
(defun-g capture-frag ((frag-pos     :vec4)
                       (frag-norm    :vec3)
                       &uniform
                       (color        :vec3)
                       (pointshadows :sampler-cube-array)
                       (scene       scene-data       :ubo)
                       (dirlights   dir-light-data   :ubo)
                       (spotlights  spot-light-data  :ubo)
                       (pointlights point-light-data :ubo))
  (let ((final-color (v! 0 0 0))
        (frag-pos3 (s~ frag-pos :xyz)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions)
          dirlights             ; TODO: SHADOW and light in clipspace?
        (incf final-color
              (dir-light-apply color (aref colors i) (aref positions i)
                               frag-pos3 frag-norm))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (point-light-apply color (aref colors i) (aref positions i)
                                    frag-pos3 frag-norm
                                    (aref linear i) (aref quadratic i))
                 (shadow-factor pointshadows frag-pos3
                                (aref positions i)
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction)
          spotlights ;; TODO: shadow
        (incf final-color
              (spot-light-apply color (aref colors i) (aref positions i) (aref direction i)
                                frag-pos3 frag-norm
                                (aref linear i) (aref quadratic i)
                                (aref cutoff i) (aref outer-cutoff i)))))
    (v! final-color 1)))

(defpipeline-g capture-pipe ()
  :vertex   (capture-vert g-pnt)
  :geometry (capture-geom (:vec3 3))
  :fragment (capture-frag :vec4 :vec3))

;;--------------------------------------------------

(defun-g capture-cube-frag ((frag-pos  :vec4)
                            (frag-norm :vec3) ;; keeping it to reuse other shaders
                            &uniform
                            (cube-sam  :sampler-cube))
  (v! (s~ (texture cube-sam (s~ frag-pos :xyz)) :xyz) 1))

(defpipeline-g capture-cube-pipe ()
  :vertex   (capture-vert g-pnt)
  :geometry (capture-geom (:vec3 3))
  :fragment (capture-cube-frag :vec4 :vec3))
