(in-package #:scenic)

(defclass directional (orth light)
  ((drawp   :initarg :drawp   :accessor drawp
            :documentation "if TRUE would draw the scene from the light POV"))
  (:default-initargs
   :drawp T
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:metaclass counted-class)
  (:documentation "simple directional light"))

(defstruct-g (dir-light-data :layout :std-140)
  (positions  (:vec3 2) :accessor positions)
  (lightspace (:mat4 2) :accessor lightspace)
  (colors     (:vec3 2) :accessor colors)
  (size        :uint    :accessor size))

(defun reset-directional-counter ()
  (setf (slot-value (find-class 'directional) 'counter) 0))
(defun current-directional-counter ()
  (slot-value (find-class 'directional) 'counter))
(defmethod initialize-instance :after ((obj directional) &key)
  (setf (slot-value obj 'idx) (current-directional-counter)))

(defmethod (setf fs)   :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf far)  :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf fov)  :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf near) :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))

(defmethod print-object ((obj directional) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos near far) obj
      (format stream "(~a ~a ~a) NEAR:~a FAR:~a" (x pos) (y pos) (z pos) near far))))

(defmethod upload ((obj directional))
  (with-slots (pos color ubo idx) obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (let ((e (aref-c c 0)))
        (setf (aref-c (lightspace e) idx) (world->clip obj))
        (setf (aref-c (positions  e) idx) pos)
        (setf (aref-c (colors     e) idx) color)))))

(defmethod init-light :after ((obj directional) ubo tex)
  (let ((idx (idx obj)))
    (log4cl:log-info "~a IDX:~a" obj idx)
    (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx))))))

(defun-g vert ((vert g-pnt) &uniform
               (model-world :mat4)
               (world-view  :mat4)
               (view-clip   :mat4)
               (scale       :float))
  (let* ((pos        (* scale (pos vert)))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (tex        (tex vert))
         (norm       (norm vert))
         (world-norm (* (m4:to-mat3 model-world) norm)))
    (values clip-pos tex world-norm (s~ world-pos :xyz))))

(defun-g simplest-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (values))

(defpipeline-g simplest-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))

(defmethod draw (actor (camera directional) time)
  (with-slots (buf scale) actor
    (map-g #'simplest-3d-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale)))

(defmethod draw :around ((obj scene) (light directional) _)
  (when (drawp light)
    (call-next-method)
    (setf (drawp light) NIL)))

(defmethod draw ((obj scene) (light directional) time)
  ;;(with-setf (cull-face) :front)
  (let ((fbo (fbo light)))
    (with-fbo-bound (fbo :attachment-for-size :d)
      (clear-fbo fbo :d)
      (dolist (a (actors obj))
        (draw a light time)))))

(defun make-directional (&rest args)
  (apply #'make-instance 'directional args))

(defun-g shadow-factor ((light-sampler      :sampler-2d-array)
                        (pos-in-light-space :vec4)
                        (bias               :float)
                        (light-index        :uint))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (current-depth (z proj-coords))
         (closest-depth (x (texture light-sampler (v! (s~ proj-coords :xy) light-index))))
         (shadow        (step (- current-depth bias) closest-depth)))
    (if (> current-depth 1)
        1f0
        shadow)))
