(in-package #:scenic)

(defclass directional (orth light)
  ()
  (:default-initargs
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "simple directional light"))

(defstruct-g (dir-light-data :layout :std-140)
  (positions  (:vec3 2) :accessor positions)
  (lightspace (:mat4 2) :accessor lightspace)
  (colors     (:vec3 2) :accessor colors)
  (fudge      (:float 2)))

(defmethod (setf fs)   :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf far)  :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf fov)  :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf near) :after (_ (obj directional)) (setf (uploadp obj) T (drawp obj) T))

(defmethod print-object ((obj directional) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos near far) obj
      (format stream "(~$ ~$ ~$) NEAR:~a FAR:~a" (x pos) (y pos) (z pos) near far))))

(defmethod upload ((obj directional))
  (with-slots (pos color ubo idx fudge) obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (let ((e (aref-c c 0)))
        (setf (aref-c (dir-light-data-fudge e) idx) fudge)
        (setf (aref-c (lightspace e) idx) (world->clip obj))
        (setf (aref-c (positions  e) idx) pos)
        (setf (aref-c (colors     e) idx) color)))))

(defmethod init-light ((obj directional) idx)
  (log4cl:log-info "IDX:~a" idx)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref (dir-tex *state*) :layer idx)))))

(defmethod initialize-instance :after ((obj directional) &key)
  (setf (slot-value obj 'ubo) (dir-ubo *state*)))

(defun-g shadow-vert ((vert g-pnt) &uniform (model->clip :mat4) (scale :float))
  (let ((pos (* scale (pos vert))))
    (* model->clip (v! pos 1))))

(defun-g shadow-frag ()
  (values))

(defpipeline-g shadow-pipe ()
  :vertex   (shadow-vert g-pnt)
  :fragment (shadow-frag))

(defmethod paint (scene (camera directional) actor time)
  (with-slots (buf scale) actor
    (map-g #'shadow-pipe buf
           :model->clip (model->clip actor camera)
           :scale scale)))

(defun-g vert-bones ((vert  g-pnt)
                     (tb    tb-data)
                     (bones assimp-bones)
                     &uniform
                     (offsets    (:mat4 69)) ;; FIXME
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4)
                     (scale       :float))
  (let* ((bone-transform
           (with-slots (weights ids) bones
             (+ (* (x weights) (aref offsets (int (x ids))))
                (* (y weights) (aref offsets (int (y ids))))
                (* (z weights) (aref offsets (int (z ids))))
                (* (w weights) (aref offsets (int (w ids)))))))
         (model-pos (pos vert))
         (world-pos
           (* model-world
              (m4:scale (v3! scale))
              bone-transform
              (v! model-pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (tex        (tex vert))
         (norm       (norm vert))
         (world-norm (* (m4:to-mat3 model-world) norm)))
    (values clip-pos tex world-norm (s~ world-pos :xyz))))

(defun-g simplest-3d-frag ((uv        :vec2)
                           (frag-norm :vec3)
                           (frag-pos  :vec3))
  (values))

(defpipeline-g simplest-3d-bones-pipe ()
  :vertex   (vert-bones g-pnt tb-data assimp-bones)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))

(defmethod paint (scene (camera directional) (actor assimp-thing-with-bones) time)
  (with-slots (buf scale bones) actor
    (map-g #'simplest-3d-bones-pipe buf
           :offsets bones
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale)))

(defmethod draw :around ((obj scene) (light directional) _)
  (when (drawp light)
    (call-next-method)
    (setf (drawp light) NIL)))

(defmethod draw ((scene scene) (light directional) time)
  ;;(with-setf (cull-face) :front)
  (let ((fbo (fbo light)))
    (with-fbo-bound (fbo :attachment-for-size :d)
      (clear-fbo fbo :d)
      (dolist (a (actors scene))
        (paint scene light a time)))))

(defun make-directional (&rest args)
  (apply #'make-instance 'directional args))

(defmethod point-at ((light directional) point-at near-factor far-factor &aux (pos (pos light)))
  (setf (near light) (* near-factor (v3:distance pos point-at))); smaller ~ 0.99
  (setf (far  light) (*  far-factor (v3:distance pos point-at))); bigger  ~ 1.05
  (setf (rot  light) (q:point-at (v! 0 1 0) pos point-at))
  (values (near light) (far light)))

(defun make-directional-point-at (point-at near-factor far-factor &rest args)
  (serapeum:lret ((light (apply #'make-instance 'directional args)))
    (point-at light point-at near-factor far-factor)))

(defun directional-p (obj) (typep obj 'directional))

(defun-g shadow-factor ((light-sampler      :sampler-2d-array)
                        (pos-in-light-space :vec4)
                        (bias               :float)
                        (light-index        :uint))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (current-depth (z proj-coords))
         (closest-depth (x (texture light-sampler (v! (s~ proj-coords :xy) light-index))))
         (shadow        (step (- current-depth bias)
                              closest-depth)))
    (if (> current-depth 1)
        1f0
        shadow)))

;; PCF
(defun-g shadow-factor ((light-sampler      :sampler-2d-array)
                        (pos-in-light-space :vec4)
                        (bias               :float)
                        (light-index        :uint))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (current-depth (z proj-coords))
         (uv (s~ proj-coords :xy))
         (texel-size (s~ (/ 1f0 (texture-size light-sampler 0))
                         :xy))
         (shadow 0f0))
    (unless (> current-depth 1)
      (for (x -1) (<= x 1) (++ x)
           (for (y -1) (<= y 1) (++ y)
                (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                       (pcf-depth (x (texture light-sampler
                                              (v! uv+offset light-index)))))
                  (incf shadow (step pcf-depth (- current-depth bias)))))))
    (- 1 (/ shadow 9f0))))
