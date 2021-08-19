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

(defmethod (setf linear) :after (new-val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-linear (aref-c c 0)) (idx obj)) new-val)))
(defmethod (setf quadratic) :after (new-val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-quadratic (aref-c c 0)) (idx obj)) new-val)))

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

(defun upload-projection (light)
  (with-gpu-array-as-c-array (c (ubo-data (ubo light)))
    (setf (shadow-projections-mats (aref-c (shadowspace (aref-c c 0)) (idx light)))
          (projection-mats light))))

(defmethod (setf pos)  :after (_ (obj point))
  (upload-transform obj))

(defmethod (setf far)  :after (new-value (obj point))
  (upload-projection obj)
  (upload-transform obj)
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-far (aref-c c 0)) (idx obj))
          new-value)))
(defmethod (setf near) :after (_ (obj point))
  (upload-projection obj)
  (upload-transform obj))

(defmethod init-light :after ((obj point) idx ubo tex)
  (log4cl:log-info "~a IDX: ~d" obj idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx :cube-face nil))))
  (upload-transform obj)
  (upload-projection obj)
  (setf (far obj) (far obj)))

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
