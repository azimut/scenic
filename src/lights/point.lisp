(in-package #:scenic)

(defclass point (pers light)
  ((linear    :initarg :linear    :accessor linear    :documentation "linear factor of pointlight decay")
   (quadratic :initarg :quadratic :accessor quadratic :documentation "quadratic factor of pointlight decay"))
  (:default-initargs
   :fov 90f0
   :fs (v! 1 1)
   :near .1f0
   :far 10f0
   :linear 0.14
   :quadratic 0.07)
  (:documentation "simple pointlight light"))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos near far linear quadratic) obj
      (format stream "(~a ~a ~a) L:~a Q:~a NEAR:~a FAR:~a"
              (x pos) (y pos) (z pos) linear quadratic near far))))

(defmethod upload ((obj point))
  (with-slots (pos color ubo idx linear quadratic far) obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (let ((e (aref-c c 0)))
        (setf (aref-c (lightspace e) idx) (world->clip obj))
        (setf (aref-c (positions  e) idx) pos)
        (setf (aref-c (colors     e) idx) color)
        (setf (aref-c (point-light-data-quadratic e) idx) quadratic)
        (setf (aref-c (point-light-data-linear    e) idx) linear)
        (setf (aref-c (point-light-data-far       e) idx) far)
        (setf (shadow-projections-mats (aref-c (shadowspace e) idx))
              (projection-mats obj))))))

(defmethod (setf linear)    :after (_ (obj point)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf quadratic) :after (_ (obj point)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf pos)       :after (_ (obj point)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf far)       :after (_ (obj point)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf near)      :after (_ (obj point)) (setf (uploadp obj) T (drawp obj) T))

(defun projection-mats (light)
  "Returns a list of 6 m4 projection matrices"
  (let ((projection (projection light))
        (light-pos  (pos light)))
    (list
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  1  0  0))))
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v! -1  0  0))))
     (m4:* projection (m4:look-at (v! 0  0  1) light-pos (v3:+ light-pos (v!  0  1  0))))
     (m4:* projection (m4:look-at (v! 0  0 -1) light-pos (v3:+ light-pos (v!  0 -1  0))))
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  0  0  1))))
     (m4:* projection (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  0  0 -1)))))))

(defmethod init-light :after ((obj point) idx ubo tex)
  (log4cl:log-info "~a IDX: ~d" obj idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx :cube-face nil)))))

;; NOTE: needs patched cbaggers/glsl-spec to make gl-layer a "place"
;; TODO: use SCALE
(defun-g shadowmap-point-vert ((vert g-pnt) &uniform (model->world :mat4))
  (* model->world (v! (pos vert) 1)))

(defun-g shadowmap-point-geom (&uniform (pointlights point-light-data :ubo)
                                        (index :int))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 18))
  (dotimes (face 6)
    (setf gl-layer (+ (* index 6) face))
    (dotimes (i 3)
      (let ((pos (gl-position (aref gl-in i))))
        (emit ()
              (* (aref (shadow-projections-mats (aref (shadowspace pointlights)
                                                      index))
                       face)
                 pos)
              pos)))
    (end-primitive))
  (values))

(defun-g shadowmap-point-frag ((frag-pos :vec4) &uniform
                               (pointlights point-light-data :ubo)
                               (index :int))
  (let* ((light-pos       (aref (positions pointlights) index))
         (far-plane       (aref (point-light-data-far pointlights) index))
         (light-distance (length (- (s~ frag-pos :xyz)
                                    light-pos)))
         (light-distance (/ light-distance
                            far-plane)))
    (setf gl-frag-depth light-distance)
    (values)))

(defpipeline-g shadowmap-point-pipe ()
  :vertex   (shadowmap-point-vert g-pnt)
  :geometry (shadowmap-point-geom)
  :fragment (shadowmap-point-frag :vec4))

(defmethod draw (actor (light point) time)
  (with-slots (buf) actor
    (map-g #'shadowmap-point-pipe buf
           :model->world (model->world actor)
           :pointlights (ubo light)
           :index (idx light))))

;; Naive approach, works fast
(defun-g shadow-factor ((light-sampler :sampler-cube-array)
                        (frag-pos      :vec3)
                        (light-pos     :vec3)
                        (far-plane     :float)
                        (bias          :float)
                        (index         :int))
  (let* ((frag-to-light (- frag-pos light-pos))
         (closest-depth (* (x (texture light-sampler (v! frag-to-light index)))
                           far-plane))
         (current-depth (length frag-to-light)))
    (if (> (- current-depth bias) closest-depth)
        0f0
        1f0)
    ;;(/ closest-depth far-plane)
    ))

(defun make-point (&rest args)
  (apply #'make-instance 'point args))

(defmethod draw :around ((obj scene) (light point) _)
  (when (drawp light)
    (call-next-method)
    (setf (drawp light) NIL)))

(defmethod draw ((obj scene) (light point) time)
  (let ((fbo (fbo light)))
    (with-fbo-bound (fbo :attachment-for-size :d)
      (clear-fbo fbo :d)
      (dolist (a (actors obj))
        (draw a light time)))))

;; For cube map array textures,
;;   zoffset is the first layer-face to clear,
;;   depth   is the number of layer-faces to clear.
#+nil
(cffi:with-foreign-object (col :float)
  (setf (cffi:mem-aref col :float) 0s0)
  (let* ((arr  (texref (point-tex (lights (current-scene)))))
         (format (element-type arr))
         (type (pixel-format-type (image-format->pixel-format format)))
         (dimensions (texture-base-dimensions (point-tex (lights (current-scene)))))
         (zoffset 0)
         (depth   6))
    (%gl:clear-tex-sub-image (texture-id (point-tex (lights (current-scene))))
                             0 0 0
                             zoffset
                             (first  dimensions)
                             (second dimensions)
                             depth
                             :depth-component;;format    ; ENUM
                             :unsigned-byte;;:int ;type      ; ENUM
                             col)))
