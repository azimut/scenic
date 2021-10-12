(in-package #:scenic)

(defclass renderable ()
  ((fbo          :reader   fbo)
   (tex          :reader   tex)
   (sam          :reader   sam)
   (dim          :accessor dim           :initarg :dim)
   (downscale    :accessor downscale     :initarg :downscale)
   (texture-opts :initarg  :texture-opts)
   (sample-opts  :initarg  :sample-opts))
  (:default-initargs
   :downscale 1f0
   :texture-opts '((0 :element-type :rgba32f) (:d :element-type :depth-component24))
   :sample-opts '((:wrap :clamp-to-edge) (:wrap :clamp-to-edge))
   :dim '(128 128))
  (:documentation "an fbo-sampler pair"))

(defmethod free :after ((obj renderable))
  (free (fbo obj))
  (free (tex obj)))

(defmethod (setf downscale) :before (new-value (obj renderable))
  (check-type new-value (float 0 1)))
(defmethod (setf downscale) :after (_ (obj renderable))
  (resize obj))

(defmethod (setf dim) :before (val (obj renderable))
  (check-type val list)
  (assert (length= 2 val)))
(defmethod (setf dim)   :after (_ (obj renderable))
  (resize obj))

(defgeneric resize (obj))
(defmethod resize :around ((obj renderable))
  (let ((old-dim (dimensions (first (tex obj))))
        (new-dim (list (round (* (first  (dim obj)) (downscale obj)))
                       (round (* (second (dim obj)) (downscale obj))))))
    (when (not (equal old-dim new-dim))
      (call-next-method))))

(defmethod resize ((obj renderable))
  (log4cl:log-info)
  (with-slots (dim downscale texture-opts sample-opts (old-tex tex) (old-sam sam) (old-fbo fbo)) obj
    (let* ((new-tex (alloc-textures texture-opts dim downscale))
           (new-sam (alloc-samplers new-tex sample-opts))
           (new-fbo (alloc-fbo new-tex texture-opts)))
      (rotatef new-tex old-tex)
      (rotatef new-fbo old-fbo)
      (rotatef new-sam old-sam)
      (free new-fbo)
      (free new-tex))))

(defmethod initialize-instance :before ((obj renderable) &key texture-opts sample-opts dim downscale)
  (check-type dim list)
  (check-type downscale (float 0 1))
  (assert (length= texture-opts sample-opts))
  (assert (length= 2 dim)))

(defun alloc-textures (texture-opts dim downscale)
  (loop :with x = (round (* (first  dim) downscale))
        :with y = (round (* (second dim) downscale))
        :for opt :in texture-opts
        :collect (apply #'make-texture nil (append (list :dimensions (list x y)) (rest opt)))))

(defun make-fbo-opts (textures texture-opts)
  (loop :for opt :in texture-opts
        :for tex :in textures
        :if (getf (rest opt) :cubes) ;; TODO: assumes both have/haven't cubes
          :collect (list (first opt) (texref tex :layer nil))
        :else
          :collect (list (first opt) tex)))

(defun alloc-fbo (textures texture-opts)
  (apply #'make-fbo (make-fbo-opts textures texture-opts)))

(defun alloc-samplers (textures sample-opts)
  (loop :for tex :in textures
        :for opt :in sample-opts
        :collect (apply #'sample tex opt)))

(defmethod initialize-instance :after ((obj renderable) &key texture-opts sample-opts dim downscale)
  (with-slots (fbo tex sam) obj
    (setf tex (alloc-textures texture-opts dim downscale))
    (setf sam (alloc-samplers tex sample-opts))
    (setf fbo (alloc-fbo      tex texture-opts))))
