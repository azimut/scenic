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

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos) obj
      (format stream "(~a ~a ~a)" (x pos) (y pos) (z pos)))))

(defmethod update (actor dt))

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
                     (spotlights  spot-light-data  :ubo))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (dir-pos    (vector (v! 0 0 0 0) (v! 0 0 0 0)))
         (spot-pos   (vector (v! 0 0 0 0) (v! 0 0 0 0))))
    (dotimes (i (size dirlights))
      (setf (aref dir-pos i)
            (* (aref (lightspace dirlights) i) world-pos)))
    (dotimes (i (size spotlights))
      (setf (aref spot-pos i)
            (* (aref (lightspace spotlights) i) world-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz)
            dir-pos
            spot-pos)))

;; NOTE: it needs cepl/core/textures/texture.lisp/allocate-immutable-texture
;; (:texture-cube-map-array
;;  (tex-storage-3d texture-type (texture-mipmap-levels texture) (texture-image-format texture)
;;                  width height (* 6 (texture-layer-count texture))))
(defun-g actor-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                     (dir-pos   (:vec4 2))
                     (spot-pos  (:vec4 2))
                     &uniform
                     (material     :int)
                     (materials    pbr-material        :ubo)
                     (dirlights    dir-light-data      :ubo)
                     (pointlights  point-light-data    :ubo)
                     (spotlights   spot-light-data     :ubo)
                     (cam-pos      :vec3)
                     (time         :float)
                     (color        :vec3)
                     (dirshadows   :sampler-2d-array)
                     (spotshadows  :sampler-2d-array)
                     (pointshadows :sampler-cube-array))
  (let ((final-color (v! 0 0 0))
        (shadow      0f0))
    (dotimes (i (size dirlights))
      (with-slots (colors positions) dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 (aref (pbr-material-roughness materials) material)
                                 (aref (pbr-material-metallic materials) material)
                                 color
                                 (aref (pbr-material-specular materials) material)
                                 (aref colors i))
                 (shadow-factor dirshadows (aref dir-pos i) .003 i)))))
    (dotimes (i (size pointlights))
      ;;(incf i 1)
      (with-slots (colors positions linear quadratic far) pointlights
        (incf final-color
              (* (pbr-point-lum (aref positions i) frag-pos cam-pos
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
    (dotimes (i (size spotlights))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction) spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos
                               frag-norm
                               (aref (pbr-material-roughness materials) material)
                               (aref (pbr-material-metallic materials) material)
                               color
                               (aref (pbr-material-specular materials) material)
                               (aref colors i)
                               (aref direction i)
                               (aref cutoff i)
                               (aref outer-cutoff i)
                               (aref linear i)
                               (aref quadratic i))
                 (shadow-factor spotshadows (aref spot-pos i) .003 i)))))
    ;;(incf final-color (v! .01 .01 .01))
    ;;(v3! shadow)
    (v! final-color 1)
    ))

(defpipeline-g actor-pipe ()
  :vertex (actor-vert g-pnt)
  :fragment (actor-frag :vec2 :vec3 :vec3 (:vec4 2) (:vec4 2)))

(defmethod draw ((actor actor) (camera renderable) time)
  (let* ((scene (current-scene)))
    (with-slots (buf scale color material) actor
      (map-g #'actor-pipe buf
             :cam-pos (pos camera)
             :dirshadows (dir-sam (lights scene))
             :pointshadows (point-sam (lights scene))
             :spotshadows (spot-sam (lights scene))
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :material material
             :materials (materials-ubo *state*)
             :dirlights (dir-ubo (lights scene))
             :pointlights (point-ubo (lights scene))
             :spotlights (spot-ubo (lights scene))
             :time time))))
