(in-package #:scenic)

;; References:
;; - 2013 https://simonstechblog.blogspot.com/2013/01/implementing-voxel-cone-tracing.html
;; - 2016 https://github.com/Friduric/voxel-cone-tracing/
;;        https://vimeo.com/212749785
;; - 2020 https://github.com/mcela/vxgi/

(defclass vxgi ()
  (voxel-fbo
   voxel-light
   voxel-sam
   voxel-zam
   (voxel-diffuse    :accessor voxel-diffuse
                     :initarg  :diffuse
                     :documentation "power of indirect diffuse lighting")
   (voxel-specular   :accessor voxel-specular
                     :initarg  :specular
                     :documentation "power of indirect specular lighting")
   (voxel-scale      :reader   voxel-scale)
   (voxel-bounds-min :accessor voxel-bounds-min :initarg :bounds-min)
   (voxel-bounds-max :accessor voxel-bounds-max :initarg :bounds-max))
  (:default-initargs
   :diffuse 1f0
   :specular 1f0
   :bounds-min (v! -1 -1 -1)
   :bounds-max (v! +1 +1 +1))
  (:documentation "voxel global illumination"))

(defun calculate-voxel-scale (bmin bmax &optional (offset 0.1))
  "(v! 0.95 0.95 0.95) ; when -1,+1 and offset 0.1
   (v! 1 1 1)          ; when -1,+1 and offset 0"
  (v! (/ (- 2 offset) (abs (- (x bmax) (x bmin))))
      (/ (- 2 offset) (abs (- (y bmax) (y bmin))))
      (/ (- 2 offset) (abs (- (z bmax) (z bmin))))))

(defmethod initialize-instance :before ((obj vxgi) &key bounds-min bounds-max diffuse specular)
  (check-type bounds-min rtg-math.types:vec3)
  (check-type bounds-max rtg-math.types:vec3)
  (check-type diffuse single-float)
  (check-type specular single-float)
  (assert (and (> (x bounds-max) (x bounds-min))
               (> (y bounds-max) (y bounds-min))
               (> (z bounds-max) (z bounds-min)))))

(defmethod initialize-instance :after ((obj vxgi) &key bounds-min bounds-max)
  (setf (slot-value obj 'voxel-scale)
        (calculate-voxel-scale bounds-min bounds-max))
  (with-slots (voxel-fbo voxel-light voxel-sam voxel-zam) obj
    (setf voxel-fbo (make-fbo `(:d :dimensions (64 64))))
    (setf voxel-light (make-texture
                       nil
                       :dimensions '(64 64 64)
                       :mipmap 7
                       :element-type :rgba16f))
    (setf voxel-sam (sample voxel-light :magnify-filter :nearest
                                        :wrap :clamp-to-border))
    (setf voxel-zam (sample voxel-light :magnify-filter :nearest
                                        :wrap :clamp-to-border))
    (setf (cepl.samplers::border-color voxel-sam) (v! 0 0 0 1))
    (setf (cepl.samplers::border-color voxel-zam) (v! 0 0 0 1))
    (setf (%cepl.types::%sampler-imagine voxel-sam) t)))

(defmethod (setf voxel-diffuse) :before (new-value (obj vxgi))
  (check-type new-value single-float))
(defmethod (setf voxel-specular) :before (new-value (obj vxgi))
  (check-type new-value single-float))
(defmethod (setf voxel-bounds-max) :before (new-value (obj vxgi))
  (check-type new-value rtg-math.types:vec3))
(defmethod (setf voxel-bounds-min) :before (new-value (obj vxgi))
  (check-type new-value rtg-math.types:vec3))

(defmethod (setf voxel-bounds-max) :after (new-value (obj vxgi))
  (with-slots (voxel-bounds-min) obj
    (setf (slot-value obj 'voxel-scale)
          (calculate-voxel-scale voxel-bounds-min new-value))))
(defmethod (setf voxel-bounds-min) :after (new-value (obj vxgi))
  (with-slots (voxel-bounds-max) obj
    (setf (slot-value obj 'voxel-scale)
          (calculate-voxel-scale new-value voxel-bounds-max))))

(defmethod free :after ((obj vxgi))
  (with-slots (voxel-fbo voxel-light) obj
    (free voxel-fbo)
    (free voxel-light)))

;; Scales and bias a given vector (i.e. from [-1, 1] to [0, 1])
(defun-g scale-and-bias ((p :vec3))
  (+ .5 (* .5 p)))

(defun-g inside-cube-p ((p :vec3))
  "Returns true if the point p is inside the unity cube."
  (and (< (abs (x p)) 1f0)
       (< (abs (y p)) 1f0)
       (< (abs (z p)) 1f0)))
