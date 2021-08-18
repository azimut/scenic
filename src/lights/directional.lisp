(in-package #:scenic)

(defclass directional (orth light)
  ()
  (:documentation "simple directional light"))

(defmethod (setf fs)   :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf pos)  :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf far)  :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf fov)  :after (_ (obj directional)) (upload-transform obj))
(defmethod (setf near) :after (_ (obj directional)) (upload-transform obj))

(defmethod print-object ((obj directional) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos near far) obj
      (format stream "(~a ~a ~a) NEAR:~a FAR:~a" (x pos) (y pos) (z pos) near far))))

(defmethod init-light :after ((obj directional) idx ubo tex)
  (log4cl:log-info "~a IDX: ~d" obj idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx))))
  (upload-transform obj))

(defun-g simplest-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (values (v! 0 0 1 0)))

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
