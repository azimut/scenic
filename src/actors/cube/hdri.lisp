(in-package #:scenic)

(defclass hdri (cube)
  ()
  (:documentation "hdri cubemap environment"))

(defun load-hdri (filepath &optional alpha-p)
  (declare (type boolean alpha-p))
  (log4cl:log-info "loading HDRI")
  (destructuring-bind (ptr width height components-per-pixel)
      (stbi:loadf filepath)
    (assert (and (= components-per-pixel 3)))
    (with-c-array-freed
        (ca (make-c-array-from-pointer
             (list width height)
             :vec3
             ptr))
      (make-texture ca :element-type (if alpha-p
                                         :rgba32f
                                         :rgb32f)))))

(defun make-hdri (filepath
                  &rest args
                  &aux (path (resolve-path filepath)))
  (let ((sam (or (gethash path *samplers*)
                 (setf (gethash path *samplers*)
                       (load-hdri path)))))
    (apply #'make-instance 'hdri :sam sam args)))

(defun-g sample-spherical-map ((v :vec3))
  (let* ((uv (v! (atan (z v) (x v))
                 (asin (y v))))
         (uv (* uv (v! ".1591" ".3183")))
         (uv (+ uv .5)))
    uv))

(defun-g hdri-frag ((pos   :vec3) &uniform
                    (sam   :sampler-2d)
                    (color :vec3))
  (let* ((uv (sample-spherical-map (- (normalize pos))));; !!
         (color3 (s~ (texture sam uv) :xyz)))
    (v! (* color3 color) 1)))

(defpipeline-g hdri-pipe ()
  :vertex   (cube-vert g-pnt)
  :fragment (hdri-frag :vec3))

(defmethod paint (scene camera (actor hdri) time)
  (with-slots (buf sam scale color) actor
    (map-g #'hdri-pipe buf
           :sam sam
           :color color
           :view (q:to-mat4 (q:inverse (rot camera)))
           :proj (projection  camera))))
