(in-package #:scenic)

(defclass directional (orth light)
  ()
  (:default-initargs
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "simple directional light"))

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
      (setf (aref-c (lightspace (aref-c c 0)) idx) (world->clip obj))
      (setf (aref-c (positions  (aref-c c 0)) idx) pos)
      (setf (aref-c (colors     (aref-c c 0)) idx) color))))

(defmethod init-light :after ((obj directional) idx ubo tex)
  (log4cl:log-info "~a IDX: ~d" obj idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx)))))

(defun-g simplest-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (values))

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

(defmethod draw :around ((obj scene) (light directional) _)
  (when (drawp light)
    (call-next-method)
    (setf (drawp light) NIL)))

(defmethod draw ((obj scene) (light directional) time)
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
