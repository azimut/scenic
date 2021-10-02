(in-package #:scenic)

(defclass renderable ()
  ((fbo :reader fbo :documentation "fbo to render to")
   (tex :reader tex :documentation "texture")
   (sam :reader sam :documentation "sample of the fbo texture")
   (dim :initarg :dim :accessor dim  :documentation "list pair of fbo dimensions")
   (scale :initarg :scale :accessor scale  :documentation "scale down from current window size <1.0")
   (texture-opts :initarg :texture-opts :documentation "texture options")
   (sample-opts  :initarg :sample-opts  :documentation "options for sample")   )
  (:default-initargs
   :scale 1f0
   :texture-opts '((0 :element-type :rgba32f) (:d :element-type :depth-component24))
   :sample-opts '((:wrap :clamp-to-edge) (:wrap :clamp-to-edge))
   :dim '(128 128))
  (:documentation "an fbo-sampler pair"))

(defmethod free :after ((obj renderable))
  (free (fbo obj))
  (free (tex obj)))

(defmethod (setf size) :before (new-value (obj renderable))
  (check-type new-value (float 0 1)))

(defmethod (setf dim) :before (val (obj renderable))
  (check-type val list)
  (assert (length= 2 val)))

(defmethod (setf dim)   :after (_ (obj renderable)) (resize obj))
(defmethod (setf scale) :after (_ (obj renderable)) (resize obj))

(defmethod handle ((event resize) (obj renderable))
  (print "EVENT")
  (print (width event)))

(defgeneric resize (obj))
(defmethod resize :around ((obj renderable))
  (let ((old-dim (dimensions (first (tex obj))))
        (new-dim (list (round (* (scale obj) (first (dim obj)))) (round (* (scale obj) (second (dim obj)))))))
    (when (not (equal old-dim new-dim))
      (call-next-method))))

(defmethod resize ((obj renderable))
  (log4cl:log-info)
  (with-slots (dim scale texture-opts sample-opts (old-tex tex) (old-sam sam) (old-fbo fbo)) obj
    (let* ((new-tex (alloc-textures texture-opts dim scale))
           (new-sam (alloc-samplers new-tex sample-opts))
           (new-fbo (alloc-fbos new-tex texture-opts)))
      (rotatef new-tex old-tex)
      (rotatef new-fbo old-fbo)
      (rotatef new-sam old-sam)
      (free new-fbo)
      (free new-tex))))

(defmethod initialize-instance :before ((obj renderable) &key texture-opts sample-opts dim scale)
  (check-type dim list)
  (check-type scale (float 0 1))
  (assert (length= texture-opts sample-opts))
  (assert (length= 2 dim)))

(defun alloc-textures (texture-opts dim scale)
  "returns a list textures"
  (loop :with x = (round (* (first  dim) scale))
        :with y = (round (* (second dim) scale))
        :for opt :in texture-opts
        :collect (apply #'make-texture nil (append (list :dimensions (list x y)) (rest opt)))))

(defun alloc-fbos (textures texture-opts)
  "returns 1 fbo"
  (let ((newopts (loop :for opt :in texture-opts
                       :for tex :in textures
                       :if (getf (rest opt) :cubes);; TODO: assumes both have/haven't cubes
                         :collect (list (first opt) (texref tex :layer nil))
                       :else
                         :collect (list (first opt) tex))))
    (apply #'make-fbo newopts)))

(defun alloc-samplers (textures sample-opts)
  "returns a list of samples"
  (loop :for tex :in textures
        :for opt :in sample-opts
        :collect (apply #'sample tex opt)))

(defmethod initialize-instance :after ((obj renderable) &key texture-opts sample-opts dim scale)
  (with-slots (fbo tex sam) obj
    (setf tex (alloc-textures texture-opts dim scale))
    (setf sam (alloc-samplers tex sample-opts))
    (setf fbo (alloc-fbos     tex texture-opts))))

(defmethod draw ((scene scene) (camera renderable) time)
  (dolist (a (actors scene))
    (paint scene a camera time)))

(defmethod draw :around ((obj scene) (camera renderable) time)
  (let ((fbo (fbo camera)))
    (with-fbo-bound (fbo)
      (with-setf (clear-color) (color obj)
        (clear-fbo fbo)
        (call-next-method)))))
