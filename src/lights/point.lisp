(in-package #:scenic)

(defclass point (pers light)
  ((linear    :initarg :linear    :accessor linear    :documentation "linear factor of pointlight decay")
   (quadratic :initarg :quadratic :accessor quadratic :documentation "quadratic factor of pointlight decay")
   (drawp   :initarg :drawp   :accessor drawp
            :documentation "if TRUE would draw the scene from the light POV"
            :allocation :class))
  (:default-initargs
   :drawp T
   :fov 90f0
   :fs (v! 1 1)
   :near .1f0
   :far 10f0
   :linear 0.14
   :quadratic 0.07)
  (:documentation "simple pointlight light"))

(defstruct-g (shadow-projections :layout :std-140)
  (mats (:mat4 6)))

(defstruct-g (point-light-data :layout :std-140)
  (positions   (:vec3 4) :accessor positions)
  (shadowspace (shadow-projections 4) :accessor shadowspace); 6 projections matrices
  (colors      (:vec3 4) :accessor colors)
  (linear      (:float 4))
  (quadratic   (:float 4))
  (far         (:float 4))
  (fudge       (:float 4)))

(defmethod init-light ((obj point) idx)
  (log4cl:log-info "IDX: ~d" idx)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref (point-tex *state*) :layer idx :cube-face nil)))))

(defmethod initialize-instance :after ((obj point) &key)
  (setf (slot-value obj 'ubo) (point-ubo *state*)))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos near far linear quadratic) obj
      (format stream "(~a ~a ~a) L:~a Q:~a NEAR:~a FAR:~a"
              (x pos) (y pos) (z pos) linear quadratic near far))))

(defmethod upload ((obj point))
  (with-slots (pos color ubo idx linear quadratic far fudge) obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (let ((e (aref-c c 0)))
        (setf (aref-c (positions  e) idx) pos)
        (setf (aref-c (colors     e) idx) color)
        (setf (aref-c (point-light-data-quadratic e) idx) quadratic)
        (setf (aref-c (point-light-data-linear    e) idx) linear)
        (setf (aref-c (point-light-data-far       e) idx) far)
        (setf (aref-c (point-light-data-fudge     e) idx) fudge)
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

;; TODO: use SCALE
(defun-g shadowmap-point-vert ((vert g-pnt) &uniform (model->world :mat4) (scale :float))
  (* model->world (v! (* scale (pos vert)) 1)))

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

(defun-g shadowmap-point-frag ((frag-pos :vec4) &uniform (pointlights point-light-data :ubo) (index :int))
  (let* ((light-pos      (aref (positions pointlights) index))
         (far-plane      (aref (point-light-data-far pointlights) index))
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

(defmethod paint (scene actor (light point) time)
  (with-slots (buf scale) actor
    (map-g #'shadowmap-point-pipe buf
           :scale scale
           :model->world (model->world actor)
           :pointlights (ubo light)
           :index (idx light))))

(defun-g shadowmap-point-bones-vert
    ((vert g-pnt) (tb tb-data) (bones assimp-bones)
     &uniform (model->world :mat4) (offsets (:mat4 41)) (scale :float))
  (* (m4:scale (v3! scale)) ;; FIXME
     model->world
     (+ (* (aref (assimp-bones-weights bones) 0)
           (aref offsets (int (aref (assimp-bones-ids bones) 0))))
        (* (aref (assimp-bones-weights bones) 1)
           (aref offsets (int (aref (assimp-bones-ids bones) 1))))
        (* (aref (assimp-bones-weights bones) 2)
           (aref offsets (int (aref (assimp-bones-ids bones) 2))))
        (* (aref (assimp-bones-weights bones) 3)
           (aref offsets (int (aref (assimp-bones-ids bones) 3)))))
     (v! (pos vert) 1)))

(defpipeline-g shadowmap-point-bones-pipe ()
  :vertex   (shadowmap-point-bones-vert g-pnt tb-data assimp-bones)
  :geometry (shadowmap-point-geom)
  :fragment (shadowmap-point-frag :vec4))

(defmethod paint (scene (actor assimp-thing-with-bones) (light point) time)
  (with-slots (buf scale bones) actor
    (map-g #'shadowmap-point-bones-pipe buf
           :scale scale
           :offsets bones
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

(defmethod draw ((scene scene) (light point) time)
  (let ((fbo (fbo light)))
    (with-fbo-bound (fbo :attachment-for-size :d)
      (dolist (a (actors scene))
        (paint scene a light time)))))

(defmethod point-size ((obj point) nth)
  (let* ((index    (min nth (length *point-light-params*)))
         (new-pair (nth index (reverse *point-light-params*))))
    (setf (linear obj) (y new-pair))
    (setf (quadratic obj) (z new-pair)))
  obj)

(defun make-point (&rest args)
  (apply #'make-instance 'point args))

(defun point-p (obj) (typep obj 'point))
