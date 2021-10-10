(in-package #:scenic)

(defclass particles (base-particles drawable)
  ((source :accessor source :initarg :source)
   (color  :accessor color  :initarg :color)
   (size   :accessor size   :initarg :size))
  (:default-initargs
   :shadowp NIL
   :size 0.2f0
   :source (v! 0 0 0)
   :color  (v! 1 1 1))
  (:documentation "POINT based particles, all of the same perceivable size"))

(defun make-particles (&rest args)
  (apply #'make-instance 'particles args))

(defmethod print-object ((obj particles) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (n-particles source size) obj
      (format stream "~a size:~f (~,2f ~,2f ~,2f)" n-particles
              size (x source) (y source) (z source)))))

(defmethod initialize-instance :before ((obj particles) &key size)
  (check-type size single-float))
(defmethod initialize-instance :after ((obj particles) &key size)
  (gl:point-size size))
(defmethod (setf size) :before (new-value (obj particles))
  (check-type new-value single-float))
(defmethod (setf size) :after (new-value (obj particles))
  (gl:point-size new-value))

(defun-g pupdate-vert ((pdata  pdata)
                       &uniform
                       (source :vec3)
                       (time   :float))
  (with-slots (pos life) pdata
    (let* ((time (* time .001 gl-vertex-id))
           (life life)
           (pos  pos)
           (r   (rand (vec2 time))))
      (if (>= life 1f0)          ;; Reset
          (progn
            (setf life (* 1 r))
            (setf pos (- source (/ source 2))))
          (progn ;; Update
            (setf life    (+ life .1))
            (incf (x pos) (* .05 (sin time)))
            (incf (y pos) .1;;(* .05 (tan time))
                  )
            (setf (z pos) (* .05 (cos time)))))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback (v! r .1 0)) ;; rot/scale/?
              (:feedback life)))))

(defpipeline-g pupdate-pipe (:points)
  :vertex (pupdate-vert pdata))

(defmethod update ((actor particles) dt)
  (with-slots (tfs-dst str-src source) actor
    (with-transform-feedback (tfs-dst)
      (map-g #'pupdate-pipe str-src
             :source source
             :time (* .1 (get-internal-real-time))))))

(defun-g prender-points-vert ((pdata pdata) &uniform (world-clip :mat4))
  (with-slots (pos) pdata
    (* world-clip (v! pos 1))))
(defun-g prender-points-frag (&uniform (color :vec3))
  (v! color 1))
(defpipeline-g prender-points-pipe (:points)
  :vertex   (prender-points-vert pdata)
  :fragment (prender-points-frag))

(defmethod swap-particles (obj)
  (with-slots (tfs-src tfs-dst str-src str-dst gar-src gar-dst) obj
    (rotatef tfs-src tfs-dst)
    (rotatef str-src str-dst)
    (rotatef gar-src gar-dst)))

(defmethod paint (scene (obj particles) camera time)
  (with-fbo-bound ((fbo camera))
    (clear-fbo (fbo camera))
    (with-slots (str-src color) obj
      (map-g #'prender-points-pipe str-src
             :world-clip (world->clip camera)
             :color color)))
  (swap-particles obj))
