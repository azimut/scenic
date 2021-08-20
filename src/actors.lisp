(in-package #:scenic)

(defclass actor ()
  ((pos      :initarg :pos      :accessor pos      :documentation "3d position")
   (rot      :initarg :rot      :accessor rot      :documentation "3d rotation")
   (buf      :initarg :buf      :accessor buf      :documentation "buffer stream")
   (color    :initarg :color    :accessor color    :documentation "base color")
   (scale    :initarg :scale    :accessor scale    :documentation "vextex fudge scale")
   (material :initarg :material :accessor material :documentation "material index"))
  (:default-initargs
   :material 0
   :color (v! 1 1 1)
   :pos (v! 0 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box))
  (:documentation "base object"))

(defmethod specular  ((obj actor)) (specular  (nth (material obj) (materials *state*))))
(defmethod metallic  ((obj actor)) (metallic  (nth (material obj) (materials *state*))))
(defmethod emissive  ((obj actor)) (emissive  (nth (material obj) (materials *state*))))
(defmethod roughness ((obj actor)) (roughness (nth (material obj) (materials *state*))))

(defmethod (setf specular)  (new-value (obj actor)) (setf (specular  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf metallic)  (new-value (obj actor)) (setf (metallic  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf emissive)  (new-value (obj actor)) (setf (emissive  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf roughness) (new-value (obj actor)) (setf (roughness (nth (material obj) (materials *state*))) new-value))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

(defun make-actor (&key (w 1f0) (h 1f0) (d 1f0) (pos (v! 0 0 0)) (material 0))
  (make-instance 'actor :buf (box w h d) :pos pos :material material))

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
                     (material     :int)
                     (materials    pbr-material :ubo)
                     (cam-pos      :vec3)
                     (time         :float)
                     (color        :vec3)
                     (dirshadows   :sampler-2d-array)
                     (dirlights    dir-light-data   :ubo)
                     (pointshadows :sampler-cube-array)
                     (pointlights point-light-data :ubo))
  (let ((final-color (v! 0 0 0))
        (shadow      0f0))
    ;;(incf final-color (* (v3! 0.01) color))
    (dotimes (i (size dirlights))
      (with-slots (colors positions) dirlights
        (incf final-color
              (* (dir-light-apply color (aref colors i) (aref positions i) frag-pos frag-norm)
                 (shadow-factor dirshadows (aref dir-pos i) .003 i)))))
    (dotimes (i (size pointlights))
      ;;(incf i 1)
      (with-slots (colors positions linear quadratic far) pointlights
        (incf final-color
              (*
               #+nil
               (point-light-apply
                color
                (aref colors i) (aref positions i) frag-pos frag-norm
                1f0 (aref linear i) (aref quadratic i)
                cam-pos
                (aref (pbr-material-roughness materials) material)
                (aref (pbr-material-specular  materials) material))
               (pbr-point-lum (aref positions i) frag-pos cam-pos
                              frag-norm
                              (aref (pbr-material-roughness materials) material)
                              (aref (pbr-material-metallic materials) material)
                              color
                              (aref (pbr-material-specular materials) material)
                              (aref linear i) (aref quadratic i) (aref colors i))
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

(defmethod draw ((actor actor) (camera renderable) time)
  (let* ((scene (current-scene)))
    (with-slots (buf scale color material) actor
      (map-g #'actor-pipe buf
             :cam-pos (pos camera)
             :dirshadows (dir-sam (lights scene))
             :pointshadows (point-sam (lights scene))
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :material material
             :materials (materials-ubo *state*)
             :dirlights (dir-ubo (lights scene))
             :pointlights (point-ubo (lights scene))
             :time time))))
