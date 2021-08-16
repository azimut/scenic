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
    (format stream "POS: ~a" (slot-value obj 'pos))))

(defmethod init-light :after ((obj directional) idx ubo tex)
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
