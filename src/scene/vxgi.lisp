(in-package #:scenic)

(defclass vxgi ()
  (voxel-fbo
   voxel-light
   voxel-sam
   voxel-zam
   (voxel-scale      :reader    voxel-scale)
   (voxel-bounds-min :accessor voxel-bounds-min :initarg :bounds-min)
   (voxel-bounds-max :accessor voxel-bounds-max :initarg :bounds-max))
  (:default-initargs
   :bounds-min (v! -1 -1 -1)
   :bounds-max (v! +1 +1 +1))
  (:documentation "voxel global illumination"))

(defun calculate-voxel-scale (bmin bmax)
  "(v! 0.95 0.95 0.95) ; when -1,+1 and offset 0.1
   (v! 1 1 1)          ; when -1,+1 and offset 0"
  (let ((offset 0.1))
    (v! (/ (- 2 offset) (abs (- (x bmax) (x bmin))))
        (/ (- 2 offset) (abs (- (y bmax) (y bmin))))
        (/ (- 2 offset) (abs (- (z bmax) (z bmin)))))))

(defmethod initialize-instance :before ((obj vxgi) &key bounds-min bounds-max)
  (check-type bounds-min rtg-math.types:vec3)
  (check-type bounds-max rtg-math.types:vec3)
  (assert (and (> (x bounds-max) (x bounds-min))
               (> (y bounds-max) (y bounds-min))
               (> (z bounds-max) (z bounds-min)))))

(defmethod initialize-instance :after ((obj vxgi) &key bounds-min bounds-max)
  (setf (slot-value obj 'voxel-scale)
        (calculate-voxel-scale bounds-min bounds-max))
  (with-slots (voxel-fbo voxel-light voxel-sam voxel-zam) obj
    (setf voxel-fbo (make-fbo `(:d :dimensions (64 64))))
    (setf voxel-light (make-texture
                       nil
                       :dimensions '(64 64 64)
                       :mipmap 7
                       :element-type :rgba8))
    (setf voxel-sam (sample voxel-light :magnify-filter :nearest
                                        :wrap :clamp-to-border))
    (setf voxel-zam (sample voxel-light :magnify-filter :nearest
                                        :wrap :clamp-to-border))
    (setf (cepl.samplers::border-color voxel-sam) (v! 0 0 0 1))
    (setf (cepl.samplers::border-color voxel-zam) (v! 0 0 0 1))
    (setf (%cepl.types::%sampler-imagine voxel-sam) t)))

(defmethod (setf voxel-bounds-max) :before (new-value (obj vxgi))
  (check-type new-value rtg-math.types:vec3))
(defmethod (setf voxel-bounds-min) :before (new-value (obj vxgi))
  (check-type new-value rtg-math.types:vec3))

(defmethod (setf voxel-bounds-max) :after (new-value (obj vxgi))
  (with-slots (voxel-bounds-min) obj
    (setf (slot-value obj 'voxel-scale)
          (calculate-voxel-scale voxel-bounds-min new-value))))
(defmethod (setf voxel-bounds-min) :after (new-value (obj vxgi))
  (with-slots (voxel-bounds-max) obj
    (setf (slot-value obj 'voxel-scale)
          (calculate-voxel-scale new-value voxel-bounds-max))))

(defmethod free :after ((obj vxgi))
  (with-slots (voxel-fbo voxel-light) obj
    (free voxel-fbo)
    (free voxel-light)))

(defclass scene-vxgi (scene vxgi)
  ()
  (:documentation "scene with global illumination"))

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
         (wp (vec3 0f0)))
    (dotimes (i 3)
      (setf wp (s~ (gl-position (aref gl-in i)) :xyz))
      (cond ((= (z p) axis)
             (emit () (v! (x wp) (y wp) 0 1)
                   wp
                   (aref wpos i)
                   (aref nor i) (aref uv i)))
            ((= (x p) axis)
             (emit () (v! (y wp) (z wp) 0 1)
                   wp
                   (aref wpos i)
                   (aref nor i) (aref uv i)))
            (t
             (emit () (v! (x wp) (z wp) 0 1)
                   wp
                   (aref wpos i)
                   (aref nor i) (aref uv i)))))
    (emit-vertex)
    (end-primitive)))

(defun-g inside-cube-p ((p :vec3) (e :float))
  "Returns true if the point p is inside the unity cube."
  (and (< (abs (x p)) (+ e 1f0))
       (< (abs (y p)) (+ e 1f0))
       (< (abs (z p)) (+ e 1f0))))

(defun-g point-light-strength ((color       :vec3)
                               (light-color :vec3)
                               (light-pos   :vec3)
                               (pos         :vec3)
                               (nor         :vec3)
                               (linear      :float)
                               (quadratic   :float))
  "no ambient for voxelization mainly"
  (let* ((direction   (normalize (- light-pos pos)))
         (distance    (distance light-pos pos))
         (attenuation (/ (+ 1 (* linear distance) (* quadratic distance distance))))
         (diff        (saturate (dot (normalize nor) direction))))
    (* diff light-color color attenuation)))

;; Scales and bias a given vector (i.e. from [-1, 1] to [0, 1])
(defun-g scale-and-bias ((p :vec3))
  (+ .5 (* .5 p)))

;; Not using texture
(defun-g voxelize-frag
    ((vpos :vec3)
     (pos  :vec3)
     (nor  :vec3)
     (uv   :vec2)
     ;;     (vpos :vec3);; Voxel Position
     &uniform
     (material     :int)
     (materials    pbr-material     :ubo)
     (pointshadows :sampler-cube-array)
     (dirlights    dir-light-data   :ubo)
     (pointlights  point-light-data :ubo)
     (spotlights   spot-light-data  :ubo)
     (scene        scene-data       :ubo)
     (voxel-scale  :vec3)
     (cam-pos      :vec3)
     (ithing       :image-3d)
     (color        :vec3))
  (if (not (inside-cube-p vpos 0f0))
      (return))
  (let (;; NT: we do not care about specular here, because that is view dependant
        (emissive (aref (pbr-material-emissive materials) material))
        (nor      (normalize nor))
        (final-color (vec3 0)))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (point-light-strength color
                                       (aref colors i)
                                       (aref positions i)
                                       pos
                                       nor
                                       (aref linear i)
                                       (aref quadratic i))
                 (shadow-factor pointshadows pos
                                (aref positions i) (aref far i) (aref fudge i) i)))))
    (let* ((voxel (scale-and-bias pos))
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

(let ((doit T))
  (defmethod draw ((scene scene-vxgi) (camera defered) time)
    (dolist (l (lights scene))
      (paint scene camera l time))
    (dolist (a (remove-if #'cube-p (actors scene)));; TODO: ewww!
      (paint scene camera a time))
    (when t;;doit
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
                       :pointshadows (point-sam *state*)
                       ;; Material
                       :material material
                       :materials (materials-ubo *state*)
                       ;; Lights
                       :dirlights (dir-ubo *state*)
                       :pointlights (point-ubo *state*)
                       :spotlights (spot-ubo *state*))))))
        (generate-mipmaps voxel-light)))  ))

(defun-g trace-diffuse-voxel-cone ((from        :vec3)
                                   (direction   :vec3)
                                   (voxel-light :sampler-3d))
  "Traces a diffuse voxel cone."
  (let* ((voxel-size #.(/ 1f0 64f0))
         (mipmap-hardcap 5.4)
         (max-dist
           ;;(distance (abs from) (v3! -1))
           ;;#.(sqrt 2)
           #.(sqrt 3)
           );F 1.414213=(sqrt 2);A 1.73205080757=(sqrt 3)
         ;;
         (aperture
           "0.325"
           ;;#.(max 0.1f0 (tan (* (radians 22f0) 0.5)))
           ;;".55785173935"
           ;;#.(clamp (* 3.14159265359 .5 .75 (max 0f0 .8)) 0.00174533102 3.14159265359)
           ) ;F=.325;A=.55785173935=(tan 22.5); AKA CONE_SPREAD
         ;;
         ;; Controls bleeding from close surfaces.
         ;; Low values look rather bad if using shadow cone tracing.
         ;; Might be a better choice to use shadow maps and lower this value.
         (dist
           ;;#.(* 1.5 (/ 1 64));; Shimmer
           "0.1953125"
           ;;(* 1 0.04)
           ;;(* 3.5f0 voxel-size)
           )        ; F .1953125 ; A 0.04 * voxelgiOffset("1"*100/100)
         (diam (* dist aperture))
         (direction (normalize direction))
         (acc (vec4 0f0)))
    ;; Trace
    (while (and (< dist max-dist)
                (< (w acc) 1f0))
           (let* ((c (scale-and-bias (+ from (* direction dist))))
                  (l (1+ (/ diam voxel-size)))
                  (level (log2 l))
                  (ll (* (1+ level) (1+ level)))
                  (voxel (texture-lod voxel-light
                                      c
                                      (min mipmap-hardcap level))))
             (incf acc
                   (* 0.075 ll voxel (pow (- 1f0 (w voxel)) 2f0)))
             (incf dist
                   (* ll voxel-size 2))))
    (pow (* 2f0 (s~ acc :xyz))
         (vec3 1.5))))

;; From Friduric
(defun-g orthogonal ((u :vec3))
  "Returns a vector that is orthogonal to u."
  (let ((u (normalize u))
        (v (v! "0.99146" "0.11664" "0.05832")))
    (if (> (abs (dot u v))
           "0.99999")
        (cross u (v! 0 1 0))
        (cross u v))))

;; From Armory (used instead of orthogonal)
(defun-g tangential ((n :vec3))
  (let ((t1 (cross n (v! 0 0 1)))
        (t2 (cross n (v! 0 1 0))))
    (if (> (length t1) (length t2))
        (normalize t1)
        (normalize t2))))

(defun-g indirect-diffuse-light ((wpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((isqrt2                  #.(/ (sqrt 2) 2)) ; 0.7071
         (diffuse-indirect-factor 0.52)
         ;; Angle mix (1.0f => orthogonal direction,
         ;;            0.0f => direction of normal).
         (angle-mix               0.5)
         (w                       (vec3 1)) ; cone weights
         ;; Find a base for the side cones with the normal as one
         ;; of its base vectors.
         (ortho                   (normalize (tangential normal)))
         (ortho2                  (normalize (cross ortho normal)))
         ;; Find base vectors for the corner cones too.
         (corner                  (* 0.5 (+ ortho ortho2)))
         (corner2                 (* 0.5 (- ortho ortho2)))
         ;; Find start position of trace (start with a bit of offset).
         (voxel-size              #.(/ 1f0 64f0))
         (n-offset                (* normal (+ 1 (* 4 isqrt2)) voxel-size))
         (c-origin                (+ wpos n-offset))
         ;; Accumulate indirect diffuse light.
         (acc                     (vec3 0))
         ;; We offset forward in normal direction, and backward in cone direction.
         ;; Backward in cone direction improves GI, and forward direction removes
         ;; artifacts.
         (cone-offset             "-0.01"))
    ;; Trace front cone
    (incf acc (* (x w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset normal))
                                                 normal
                                                 voxel-light)))
    ;; Trace 4 side cones.
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho))
                                                 (mix normal ortho angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho))
                                                 (mix normal (- ortho) angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho2))
                                                 (mix normal ortho2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho2))
                                                 (mix normal (- ortho2) angle-mix)
                                                 voxel-light)))
    ;; Trace 4 corner cones.
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner))
                                                 (mix normal corner angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner))
                                                 (mix normal (- corner) angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner2))
                                                 (mix normal corner2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner2))
                                                 (mix normal (- corner2) angle-mix)
                                                 voxel-light)))
    ;; Return result.
    (* diffuse-indirect-factor
       acc
       (+ albedo 0.001))
    ;;(* (+ albedo .0001) (/ acc 9))
    ))

(defun-g defered-vxgi-frag ((uv :vec2)
                            &uniform
                            (sample1      :sampler-2d)
                            (sample2      :sampler-2d)
                            (sample3      :sampler-2d)
                            (sample4      :sampler-2d)
                            (dirlights    dir-light-data      :ubo)
                            (pointlights  point-light-data    :ubo)
                            (spotlights   spot-light-data     :ubo)
                            (scene        scene-data          :ubo)
                            (voxel-scale  :vec3)
                            (cam-pos      :vec3)
                            (voxel-light  :sampler-3d)
                            (dirshadows   :sampler-2d-array)
                            (spotshadows  :sampler-2d-array)
                            (pointshadows :sampler-cube-array))
  (let* ((color1    (texture sample1 uv))
         (color2    (texture sample2 uv))
         (color3    (texture sample3 uv))
         (color4    (texture sample4 uv))
         (roughness (w color1))
         (ao        (w color2))
         (specular  (w color3))
         (metallic  (x color4))
         (emissive  (y color4))
         (color     (s~ color1 :xyz))
         (frag-pos  (s~ color2 :xyz))
         (frag-norm (s~ color3 :xyz))
         (final-color (v! 0 0 0))
         (indirect-raw (indirect-diffuse-light (* frag-pos voxel-scale)
                                               frag-norm
                                               voxel-light color))
         (indirect     (s~ indirect-raw :xyz)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 roughness
                                 metallic
                                 color
                                 specular
                                 (aref colors i))
                 (shadow-factor dirshadows (* (aref lightspace i) (v! frag-pos 1)) (aref fudge i) i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (pbr-point-lum (aref positions i) frag-pos cam-pos
                                frag-norm
                                roughness
                                metallic
                                color
                                specular
                                (aref linear i) (aref quadratic i) (aref colors i))
                 (shadow-factor pointshadows
                                frag-pos
                                (aref positions i) ;;?
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction lightspace fudge)
          spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos frag-norm
                               roughness
                               metallic
                               color
                               specular
                               (aref colors       i)
                               (aref direction    i)
                               (aref cutoff       i)
                               (aref outer-cutoff i)
                               (aref linear       i)
                               (aref quadratic    i))
                 (shadow-factor spotshadows
                                (* (aref lightspace i) (v! frag-pos 1))
                                (aref fudge i)
                                i)))))
    (v! (+ indirect
           ;;final-color
           ;;(vec3 (z indirect-raw))
           ;;(vec3 (saturate (pow (z indirect-raw) (/ 1f0 22f0))))
           )
        ;; TODO: this alpha is to blend the possible cubemap
        (- 1 (step (y color) 0f0))
        ;;1
        )))

(defpipeline-g defered-vxgi-pipe (:points)
  :fragment (defered-vxgi-frag :vec2))

(defmethod blit ((scene scene-vxgi) (postprocess list) (camera defered) time)
  (destructuring-bind (s1 s2 s3 s4 _) (sam camera)
    (declare (ignore _))
    (with-slots (prev bs) *state*
      (with-fbo-bound ((fbo prev))
        (clear-fbo (fbo prev)) ;; needed for scenes with no envmap
        (with-blending (blend camera)
          (alexandria:when-let
              ((actor (find-if #'cube-p (actors scene))))
            (paint scene camera actor time))
          (map-g #'defered-vxgi-pipe bs
                 :cam-pos (pos camera)
                 :scene (ubo scene)
                 ;; Samples
                 :voxel-scale (voxel-scale scene)
                 :voxel-light (slot-value scene 'voxel-sam)
                 :sample1 s1
                 :sample2 s2
                 :sample3 s3
                 :sample4 s4
                 ;; Lights
                 :dirlights (dir-ubo *state*)
                 :spotlights (spot-ubo *state*)
                 :pointlights (point-ubo *state*)
                 ;; Shadows
                 :dirshadows (dir-sam *state*)
                 :spotshadows (spot-sam *state*)
                 :pointshadows (point-sam *state*)))))))
