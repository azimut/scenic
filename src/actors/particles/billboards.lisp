(in-package #:scenic)

(defclass billboards (particles)
  ((sam   :initarg :sam   :reader sam)
   (blend :initarg :blend :reader blend))
  (:default-initargs
   :blend (make-blending-params)
   :sam (error "you need to define a :sam"))
  (:documentation "point->billboards"))

(defun make-billboards (&rest args)
  (apply #'make-instance 'billboards args))

(defmethod initialize-instance :before ((obj billboards) &key path)
  (assert (probe-file (resolve-path path))))

(defun-g billboard-vert ((pdata pdata) &uniform (world-view :mat4))
  (with-slots (pos life dir) pdata
    (values (* world-view (v! pos 1))
            life
            dir)))

;; https://gamedev.stackexchange.com/questions/113147/rotate-billboard-towards-camera
(defun-g billboard-geom ((life (:float 1)) (rot (:vec3 1)) &uniform (view-clip  :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let ((up    (v! (aref view-clip 0 1) (aref view-clip 1 1) (aref view-clip 2 1)))
            (right (v! (aref view-clip 0 0) (aref view-clip 1 0) (aref view-clip 2 0)))
            (scale (y (aref rot 0))))
        (emit () (* view-clip (v! (+ p (* right  0.5 scale) (* up -0.5 scale)) 1)) (v! 0 0) life)
        (emit () (* view-clip (v! (+ p (* right  0.5 scale) (* up  0.5 scale)) 1)) (v! 0 1) life)
        (emit () (* view-clip (v! (+ p (* right -0.5 scale) (* up -0.5 scale)) 1)) (v! 1 0) life)
        (emit () (* view-clip (v! (+ p (* right -0.5 scale) (* up  0.5 scale)) 1)) (v! 1 1) life)
        (end-primitive)
        (values)))))

;; https://developer.download.nvidia.com/whitepapers/2007/SDK10/SoftParticles_hi.pdf
;; https://discourse.threejs.org/t/soft-particles-render/504/3
(defun-g calculate-fade ((particle-depth :float)
                         (scene-depth    :float))
  (let* ((z-fade      1f0)
         (f-distance 10f0)
         (f-contrast  1f0)
         (input-depth (* (- scene-depth particle-depth) f-distance)))
    (if (and (> input-depth 0) (< input-depth 1))
        (setf z-fade (* .5 (pow (saturate (* 2f0 (if (> input-depth .5)
                                                     (- 1 input-depth)
                                                     input-depth)))
                                f-contrast))
              z-fade (if (> input-depth .5) (- 1 z-fade) z-fade))
        (setf z-fade (saturate input-depth)))
    z-fade))

(defun-g billboard-frag ((uv :vec2) (life :float)
                         &uniform
                         (color :vec3)
                         (res  :vec2)
                         (sam  :sampler-2d)
                         (samd :sampler-2d))
  (let* ((sprites 8)
         ;;(uv (/ uv sprites))
         (albedo (texture sam uv)))
    (v! (* (s~ albedo :xyz) color)
	(* (- 1 life)
           (w albedo)
           (calculate-fade
            (z gl-frag-coord)
            (x (texel-fetch samd
                            (ivec2 (int (round (x (s~ gl-frag-coord :xy))))
                                   (int (round (y (s~ gl-frag-coord :xy)))))
                            0)))))))

(defpipeline-g billboard-pipe (:points)
  :vertex   (billboard-vert pdata)
  :geometry (billboard-geom (:float 1) (:vec3 1))
  :fragment (billboard-frag :vec2 :float))

(defmethod paint (scene camera (obj billboards) time)
  "textured particles blended into the scene"
  ;; Use a simple mask when not using soft-particles
  ;; Otherwise, setf (depth-mask) to nil AND make sure is the first element
  (with-setf (depth-mask) nil
    (with-slots (sam blend color str-src) obj
      (with-blending blend
        (map-g #'billboard-pipe str-src
               :color color
               :sam sam
               :world-view (world->view camera)
               :view-clip  (projection  camera)
               :samd (second (sam camera))
               :res (v! (dim camera))))))
  (swap-particles obj))
