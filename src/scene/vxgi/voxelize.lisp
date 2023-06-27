(in-package #:scenic)

;; Scales and bias a given vector (i.e. from [-1, 1] to [0, 1])
(defun-g scale-and-bias ((p :vec3))
  (+ .5 (* .5 p)))

(defun-g inside-cube-p ((p :vec3))
  "Returns true if the point p is inside the unity cube."
  (and (< (abs (x p)) 1f0)
       (< (abs (y p)) 1f0)
       (< (abs (z p)) 1f0)))

(defun-g voxelize-vert ((vert g-pnt) &uniform
                        (scale       :float)
                        (model-world :mat4)
                        (voxel-scale :vec3))
  (let ((wpos (s~ (* model-world (v! (* scale (pos vert)) 1)) :xyz)))
    ;; we want to voxelize everything
    ;; so the whole world is scaled to be inside clip space (-1.0...1.0)
    (values (v! (* wpos voxel-scale) 1)
            wpos
            (normalize (* (m4:to-mat3 model-world) (norm vert)))
            (tex vert))))

(defun-g voxelize-geom ((wpos (:vec3 3))
                        (nor  (:vec3 3))
                        (uv   (:vec2 3)))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((p1 (- (s~ (gl-position (aref gl-in 1)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p2 (- (s~ (gl-position (aref gl-in 2)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p  (abs (normalize (cross p1 p2))))
         (axis (max (x p) (y p) (z p)))
         (vp (vec3 0f0)))
    (dotimes (i 3)
      (setf vp (s~ (gl-position (aref gl-in i))
                   :xyz))
      (cond ((= (z p) axis)
             (emit () (v! (x vp) (y vp) 0 1)
                   vp
                   (aref wpos i)
                   (aref nor i)
                   (aref uv i)))
            ((= (x p) axis)
             (emit () (v! (y vp) (z vp) 0 1)
                   vp
                   (aref wpos i)
                   (aref nor i)
                   (aref uv i)))
            (t
             (emit () (v! (x vp) (z vp) 0 1)
                   vp
                   (aref wpos i)
                   (aref nor i)
                   (aref uv i)))))
    (emit-vertex)
    (end-primitive)))

;; Not using texture
(defun-g voxelize-frag
    ((vpos :vec3)
     (pos  :vec3)
     (nor  :vec3)
     (uv   :vec2)
     &uniform
     (material     :int)
     (materials    pbr-material     :ubo)
     (dirshadows   :sampler-2d-array)
     (spotshadows  :sampler-2d-array)
     (pointshadows :sampler-cube-array)
     (dirlights    dir-light-data   :ubo)
     (pointlights  point-light-data :ubo)
     (spotlights   spot-light-data  :ubo)
     (scene        scene-data       :ubo)
     (voxel-scale  :vec3)
     (cam-pos      :vec3)
     (ithing       :image-3d)
     (color        :vec3))
  (if (not (inside-cube-p vpos))
      (return))
  (let (;; NT: we do not care about specular here, because that is view dependant
        (emissive  (aref (pbr-material-emissive  materials) material))
        (metallic  (aref (pbr-material-metallic  materials) material))
        (roughness (aref (pbr-material-roughness materials) material))
        (nor       (normalize nor))
        (final-color (vec3 0)))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (pbr-point-lum (aref positions i)
                                pos
                                (+ pos .1) ;; Add some random non-zero value
                                nor
                                roughness
                                metallic
                                color
                                0 ;; No specular strength
                                (aref linear i)
                                (aref quadratic i)
                                (aref colors i))
                 (shadow-factor pointshadows pos
                                (aref positions i)
                                (aref far i)
                                (aref fudge i) i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction lightspace fudge)
          spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) pos (+ pos .1) nor
                               roughness
                               metallic
                               color
                               0
                               (aref colors       i)
                               (aref direction    i)
                               (aref cutoff       i)
                               (aref outer-cutoff i)
                               (aref linear       i)
                               (aref quadratic    i))
                 (shadow-factor spotshadows
                                (* (aref lightspace i) (v! pos 1))
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) pos (+ pos .1) nor
                                 roughness
                                 metallic
                                 color
                                 0
                                 (aref colors i))
                 (shadow-factor dirshadows (* (aref lightspace i) (v! pos 1))
                                (aref fudge i) i)))))
    (let* ((voxel (scale-and-bias vpos))
           (dim   (image-size ithing))
           (dxv   (* voxel dim))
           (res   (v! final-color 1))
           (coord (ivec3 (int (x dxv))
                         (int (y dxv))
                         (int (z dxv)))))
      (image-store ithing coord res)
      (values))))

(defpipeline-g voxelize-pipe ()
  :vertex   (voxelize-vert g-pnt)
  :geometry (voxelize-geom (:vec3 3) (:vec3 3) (:vec2 3))
  :fragment (voxelize-frag :vec3 :vec3 :vec3 :vec2))

(defun-g voxelize-tex-frag
    ((vpos :vec3)
     (pos  :vec3)
     (nor  :vec3)
     (uv   :vec2)
     &uniform
     (albedo       :sampler-2d)
     (material     :int)
     (materials    pbr-material     :ubo)
     (dirshadows   :sampler-2d-array)
     (spotshadows  :sampler-2d-array)
     (pointshadows :sampler-cube-array)
     (dirlights    dir-light-data   :ubo)
     (pointlights  point-light-data :ubo)
     (spotlights   spot-light-data  :ubo)
     (scene        scene-data       :ubo)
     (voxel-scale  :vec3)
     (cam-pos      :vec3)
     (ithing       :image-3d))
  (if (not (inside-cube-p vpos))
      (return))
  (let (;; NT: we do not care about specular here, because that is view dependant
        (emissive  (aref (pbr-material-emissive  materials) material))
        (metallic  (aref (pbr-material-metallic  materials) material))
        (roughness (aref (pbr-material-roughness materials) material))
        (color     (s~ (texture albedo uv) :xyz))
        (nor       (normalize nor))
        (final-color (vec3 0)))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (pbr-point-lum (aref positions i)
                                pos
                                (+ pos .1) ;; Add some random non-zero value
                                nor
                                roughness
                                metallic
                                color
                                0 ;; No specular strength
                                (aref linear i)
                                (aref quadratic i)
                                (aref colors i))
                 (shadow-factor pointshadows pos
                                (aref positions i)
                                (aref far i)
                                (aref fudge i) i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction lightspace fudge)
          spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) pos (+ pos .1) nor
                               roughness
                               metallic
                               color
                               0
                               (aref colors       i)
                               (aref direction    i)
                               (aref cutoff       i)
                               (aref outer-cutoff i)
                               (aref linear       i)
                               (aref quadratic    i))
                 (shadow-factor spotshadows
                                (* (aref lightspace i) (v! pos 1))
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) pos (+ pos .1) nor
                                 roughness
                                 metallic
                                 color
                                 0
                                 (aref colors i))
                 (shadow-factor dirshadows (* (aref lightspace i) (v! pos 1))
                                (aref fudge i) i)))))
    (let* ((voxel (scale-and-bias vpos))
           (dim   (image-size ithing))
           (dxv   (* voxel dim))
           (res   (v! final-color 1))
           (coord (ivec3 (int (x dxv))
                         (int (y dxv))
                         (int (z dxv)))))
      (image-store ithing coord res)
      (values))))

(defpipeline-g voxelize-tex-pipe ()
  :vertex   (voxelize-vert g-pnt)
  :geometry (voxelize-geom (:vec3 3) (:vec3 3) (:vec2 3))
  :fragment (voxelize-tex-frag :vec3 :vec3 :vec3 :vec2))

(let ((doit T))
  (defmethod draw ((scene scene-vxgi) (camera defered) time)
    (dolist (l (lights scene))
      (paint scene camera l time))
    (dolist (a (remove-if #'cube-p (actors scene))) ;; TODO: ewww!
      (paint scene camera a time))
    (when t ;;doit
      (setf doit NIL)
      (with-slots (voxel-fbo voxel-light voxel-scale) scene
        (%gl:clear-tex-image (texture-id voxel-light)
                             0
                             :rgba
                             :float
                             (cffi:null-pointer))
        (with-fbo-bound (voxel-fbo :attachment-for-size :d)
          (with-setf* ((depth-test-function) nil
                       ;;(clear-color) (v! 1 1 1 1)
                       (depth-mask) nil
                       (cull-face) nil)
            (dolist (actor (actors scene))
              (typecase actor
                (albedoed
                 (with-slots (buf scale albedo material) actor
                   (map-g #'voxelize-tex-pipe buf
                          :scene (ubo scene)
                          :voxel-scale voxel-scale
                          ;; - Vertex
                          :scale scale
                          :model-world (model->world actor)
                          ;; - Fragment
                          :albedo albedo
                          :cam-pos (pos camera)
                          :ithing (slot-value scene 'voxel-sam)
                          ;; Shadows
                          :dirshadows (dir-sam *state*)
                          :pointshadows (point-sam *state*)
                          :spotshadows  (spot-sam *state*)
                          ;; Material
                          :material material
                          :materials (materials-ubo *state*)
                          ;; Lights
                          :dirlights (dir-ubo *state*)
                          :pointlights (point-ubo *state*)
                          :spotlights (spot-ubo *state*))))
                (t
                 (with-slots (buf scale color material) actor
                   (map-g #'voxelize-pipe buf
                          :scene (ubo scene)
                          :voxel-scale voxel-scale
                          ;; - Vertex
                          :scale scale
                          :model-world (model->world actor)
                          ;; - Fragment
                          :color color
                          :cam-pos (pos camera)
                          :ithing (slot-value scene 'voxel-sam)
                          ;; Shadows
                          :dirshadows (dir-sam *state*)
                          :pointshadows (point-sam *state*)
                          :spotshadows  (spot-sam *state*)
                          ;; Material
                          :material material
                          :materials (materials-ubo *state*)
                          ;; Lights
                          :dirlights (dir-ubo *state*)
                          :pointlights (point-ubo *state*)
                          :spotlights (spot-ubo *state*))))))))
        (generate-mipmaps voxel-light)))))
