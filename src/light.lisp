(in-package #:scenic)

(defclass lights ()
  ((dir-lights   :reader dir-lights   :documentation "list of directional lights"   :initarg :dir-lights)
   (dir-tex      :reader dir-tex      :documentation "common shadow textures array")
   (dir-sam      :reader dir-sam      :documentation "common shadow samplers array")
   (dir-ubo      :reader dir-ubo      :documentation "common directional lights ubo")
   (point-lights :reader point-lights :documentation "list of point lights"         :initarg :point-lights)
   (point-tex    :reader point-tex    :documentation "common shadow textures array")
   (point-sam    :reader point-sam    :documentation "common shadow samplers array")
   (point-ubo    :reader point-ubo    :documentation "common point lights ubo")
   (dim          :reader dim          :documentation "shadow dimensions NxN"        :initarg :dim))
  (:default-initargs
   :dim 1024
   :dir-lights ()
   :point-lights ())
  (:documentation "light resources for the scene"))

(defmethod print-object ((obj lights) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "DIM:~a D: ~a P: ~a"
            (slot-value obj 'dim)
            (length (slot-value obj 'dir-lights))
            (length (slot-value obj 'point-lights)))))

(defstruct-g (dir-light-data :layout :std-140)
  (positions  (:vec3 3) :accessor positions)
  (lightspace (:mat4 3) :accessor lightspace)
  (colors     (:vec3 3) :accessor colors)
  (size        :uint    :accessor size))

(defstruct-g (point-light-data :layout :std-140)
  (positions  (:vec3 5) :accessor positions)
  (lightspace (:mat4 5) :accessor lightspace)
  (colors     (:vec3 5) :accessor colors)
  (linear     (:float 5))
  (quadratic  (:float 5))
  (size        :uint    :accessor size))

(defmethod free ((obj lights))
  (with-slots (dir-tex dir-ubo point-ubo dir-lights point-lights) obj
    (free dir-tex)
    (free dir-ubo)
    (free point-ubo)
    (mapc #'free dir-lights)
    (mapc #'free point-lights)))

(defmethod initialize-instance :before ((obj lights) &key dim dir-lights point-lights)
  (check-type dir-lights list)
  (check-type point-lights list)
  (check-type dim (integer 256 4096)))

(defmethod initialize-instance :after ((obj lights) &key dim dir-lights point-lights)
  (with-slots (dir-tex dir-sam dir-ubo point-ubo) obj
    (setf dir-tex   (make-texture NIL :dimensions `(,dim ,dim) :layer-count 3 :element-type :depth-component24))
    (setf dir-sam   (sample dir-tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest))
    (setf (cepl.samplers::border-color dir-sam) (v! 1 1 1 1))
    (setf dir-ubo   (make-ubo NIL 'dir-light-data))
    (setf point-ubo (make-ubo NIL 'point-light-data))
    (with-gpu-array-as-c-array (c (ubo-data dir-ubo))
      (setf (size (aref-c c 0)) (length dir-lights)))
    (with-gpu-array-as-c-array (c (ubo-data point-ubo))
      (setf (size (aref-c c 0)) (length point-lights)))
    (init-collection dir-lights   dir-ubo   dir-tex)
    (init-collection point-lights point-ubo dir-tex)))

(defun init-collection (lights ubo tex)
  "takes care of calling each individual light initialization, once we know their IDX"
  (let ((idx 0))
    (dolist (light lights)
      (init-light light idx ubo tex)
      (incf idx))))

(defgeneric init-light (obj idx ubo tex))

(defmethod draw ((obj scene) (lights lights) time)
  (dolist (i (dir-lights lights))   (draw obj i time))
  ;;(dolist (i (point-lights lights)) (draw obj i time))
  )

(defun make-lights (&rest args)
  (apply #'make-instance 'lights args))

(defclass light ()
  ((color :initarg :color :accessor color :documentation "light color")
   (fbo :reader fbo :documentation "light camera fbo")
   (idx :reader idx :documentation "light index on texture")
   (ubo :reader ubo :documentation "reference to scene ubo with light data"))
  (:default-initargs
   :color (v! 1 1 1)
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "base class for all lights"))

(defmethod (setf pos) :after (val (obj light))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (positions (aref-c c 0)) (idx obj)) val)))
(defmethod (setf color) :after (val (obj light))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (colors (aref-c c 0)) (idx obj)) val)))

(defmethod init-light ((obj light) idx ubo tex)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'ubo) ubo)
  (with-gpu-array-as-c-array (c (ubo-data ubo))
    (setf (aref-c (positions (aref-c c 0)) idx) (pos   obj))
    (setf (aref-c (colors    (aref-c c 0)) idx) (color obj))))

(defmethod draw ((obj scene) (light light) time)
  (let ((fbo (fbo light)))
    (with-setf (cull-face) :front
      (with-fbo-bound (fbo :attachment-for-size :d)
        (clear-fbo fbo :d)
        (dolist (a (actors obj))
          (draw a light time))))))

(defclass directional (orth light)
  ()
  (:documentation "simple directional light"))

(defmethod update ((obj directional) dt)
  (let* ((new-pos (v3:*s (v! -50 30 50) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    ;;(setf (pos obj) new-pos)
    ;;(setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    ;; (setf (far obj)  (+ new-dis (* new-dis .1)))
    ;; (setf (near obj) (- new-dis (* new-dis .01)))
    ;;(setf (fs obj) (v2! 9))
    ))

(defun upload-transform (light)
  "uploads to the fbo the light matrix"
  (with-slots (ubo idx) light
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (aref-c (lightspace (aref-c c 0)) idx)
            (world->clip light)))))

(defmethod (setf fs)   :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf pos)  :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf far)  :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf fov)  :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf near) :after (_ (obj directional)) (upload-transform obj))

(defmethod print-object ((obj directional) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "POS: ~a" (slot-value obj 'pos))))

(defun make-directional (&rest args)
  (apply #'make-instance 'directional args))

(defmethod init-light :after ((obj directional) idx ubo tex)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx))))
  (upload-transform obj))

(defclass point (orth light)
  ((linear    :initarg :linear    :accessor linear    :documentation "linear factor of pointlight decay")
   (quadratic :initarg :quadratic :accessor quadratic :documentation "quadratic factor of pointlight decay"))
  (:default-initargs
   :linear 0.14
   :quadratic 0.07)
  (:documentation "simple pointlight light"))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "POS: ~a LINEAR: ~a QUADRATIC: ~a"
            (slot-value obj 'pos)
            (slot-value obj 'linear)
            (slot-value obj 'quadratic))))

(defmethod (setf linear) :after (val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-linear (aref-c c 0)) (idx obj)) val)))
(defmethod (setf quadratic) :after (val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-quadratic (aref-c c 0)) (idx obj)) val)))

(defun make-point (&rest args)
  (apply #'make-instance 'point args))

;; TODO: init fbo for pointlight
(defun-g simplest-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (v! 0 0 1 0))

(defpipeline-g simplest-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))

(defmethod draw (actor (camera directional) time)
  "Simple pass to draw actor from light's POV"
  (with-setf (cull-face) :front)
  (with-slots (buf scale) actor
    (map-g #'simplest-3d-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale)))
