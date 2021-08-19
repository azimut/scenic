(in-package #:scenic)

(defclass actor ()
  ((pos   :initarg :pos   :accessor pos   :documentation "3d position")
   (rot   :initarg :rot   :accessor rot   :documentation "3d rotation")
   (buf   :initarg :buf   :accessor buf   :documentation "buffer stream")
   (color :initarg :color :accessor color :documentation "base color")
   (scale :initarg :scale :accessor scale :documentation "vextex fudge scale"))
  (:default-initargs
   :color (v! 1 1 1)
   :pos (v! 0 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box))
  (:documentation "base object"))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

(defun make-actor (&key (w 1f0) (h 1f0) (d 1f0) (pos (v! 0 0 0)))
  (make-instance 'actor :buf (box w h d) :pos pos))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))

(defun-g actor-vert ((vert g-pnt) &uniform
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4)
                     (scale       :float)
                     (dirlights   dir-light-data   :ubo)
                     (pointlights point-light-data :ubo))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (dir-pos    (vector (v! 0 0 0 0) (v! 0 0 0 0) (v! 0 0 0 0)))
         (point-pos  (vector (v! 0 0 0 0) (v! 0 0 0 0) (v! 0 0 0 0) (v! 0 0 0 0) (v! 0 0 0 0))))
    (dotimes (i (size dirlights))
      (setf (aref dir-pos i)
            (* (aref (lightspace dirlights) i) world-pos)))
    (dotimes (i (size pointlights))
      (setf (aref point-pos i)
            (* (aref (lightspace pointlights) i) world-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz)
            dir-pos
            point-pos)))

;; NOTE: it needs cepl/core/textures/texture.lisp/allocate-immutable-texture
;; (:texture-cube-map-array
;;  (tex-storage-3d texture-type (texture-mipmap-levels texture) (texture-image-format texture)
;;                  width height (* 6 (texture-layer-count texture))))
(defun-g actor-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                     (dir-pos   (:vec4 3))
                     (point-pos (:vec4 5))
                     &uniform
                     (time         :float)
                     (color        :vec3)
                     (dirshadows   :sampler-2d-array)
                     (dirlights    dir-light-data   :ubo)
                     (pointshadows :sampler-cube-array)
                     (pointlights point-light-data :ubo))
  (let ((final-color (v! 0 0 0))
        (shadow 0f0))
    (dotimes (i (size dirlights))
      (with-slots (colors positions) dirlights
        (incf final-color
              (* (dir-light-apply color (aref colors i) (aref positions i) frag-pos frag-norm)
                 (shadow-factor dirshadows (aref dir-pos i) .003 i)))))
    (dotimes (i 0;(size pointlights)
                )
      (with-slots (colors positions linear quadratic far) pointlights
        (incf final-color
              (* (point-light-apply color
                                    (aref colors i) (aref positions i) frag-pos frag-norm
                                    (aref linear i) (aref quadratic i))
                 (shadow-factor pointshadows
                                frag-pos
                                (aref positions i)
                                (aref far i)
                                .03
                                i)))))
    ;;(incf final-color (v! .01 .01 .01))
    ;;(v3! shadow)
    (v! final-color 1)
    ))

(defpipeline-g actor-pipe ()
  :vertex (actor-vert g-pnt)
  :fragment (actor-frag :vec2 :vec3 :vec3 (:vec4 3) (:vec4 5)))

(let ((stepper (make-stepper (seconds 5) (seconds 5))))
  (defmethod update ((actor actor) dt)
    #+nil
    (when (funcall stepper)
      (setf (rot actor)
            (q:* (q:from-axis-angle (v! 1 0 0) (radians (random 90)))
                 (q:from-axis-angle (v! 0 0 1) (radians (random 90)))))
      (setf (pos actor) (v! (+ -2.5 (* 5 (sin (mynow)))) 0
                            (+ -2.5 (* 5 (cos (mynow)))) 0)))))

(defmethod draw ((actor actor) (camera renderable) time)
  (let* ((scene (current-scene)))
    (with-slots (buf scale color) actor
      (map-g #'actor-pipe buf
             :dirshadows (dir-sam (lights scene))
             :pointshadows (point-sam (lights scene))
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :dirlights (dir-ubo (lights scene))
             :pointlights (point-ubo (lights scene))
             :time time))))
