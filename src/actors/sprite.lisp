(in-package #:scenic)

;; TODO: position, rotation and might be 3d
;; TODO: real time depth test for all sprites to know the render order

(defclass sprite (uploadable)
  ((nrows :initarg :nrows :accessor sprite-rows  :documentation "number of cols in the sprite sheet")
   (ncols :initarg :ncols :accessor sprite-cols  :documentation "number of cols in the sprite sheet")
   (index :initarg :index :accessor sprite-index :documentation "the sprite number")
   (flipx :initarg :flipx :accessor sprite-flipx :documentation "flip sprite along the X axis")
   (flipy :initarg :flipy :accessor sprite-flipy :documentation "flip sprite along the Y axis")
   (scale :initarg :scale :accessor sprite-scale :documentation "scale of the single frame")
   (ubo   :reader ubo :documentation "UBO used to upload values to gpu")
   (fbo   :reader fbo :documentation "FBO that captures the sprite")
   (tex   :reader tex :documentation "TEXTURE that captures the sprite")
   (sam   :reader sam :documentation "output SAMPLER of the selected sprite cell")
   (isam  :initarg :isam
          :reader sprite-isam
          :documentation "input spritesheet sampler")
   (blend :reader sprite-blend
          :allocation :class
          :documentation "used to properly capture the alpha?"))
  (:default-initargs
   :isam (error "you need to provide :isam for the input sampler of the sprite sheet")
   :scale 1f0
   :nrows 1
   :ncols 1
   :index 0
   :flipx T
   :flipy T)
  (:documentation "renders the selected cell of a sprite sheet, assumes a squared sheet"))

(defmethod print-object ((obj sprite) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (nrows ncols index flipx flipy) obj
      (format stream "~dx~d I=~d ~a/~a"
              nrows ncols index flipx flipy))))

(defstruct-g (sprite-data :layout :std-140)
  (scale :float)
  (nrows :int)
  (ncols :int)
  (index :int)
  (flipx :int)
  (flipy :int))

(defmethod free :after ((obj sprite))
  (free (fbo obj))
  (free (tex obj))
  (free (ubo obj)))

(defmethod initialize-instance
    :before ((obj sprite)
             &key flipx flipy nrows ncols index scale isam)
  (check-type isam %cepl.types:sampler)
  (check-type scale (single-float 0f0 1024f0))
  (check-type index (integer 0 1024))
  (check-type ncols (integer 1 1024))
  (check-type nrows (integer 1 1024))
  (check-type flipx boolean)
  (check-type flipy boolean))

(defmethod (setf sprite-scale) :before (new-value (obj sprite))
  (check-type new-value (single-float 0f0 1024f0)))
(defmethod (setf sprite-index) :before (new-value (obj sprite))
  (check-type new-value integer))
(defmethod (setf sprite-nrows) :before (new-value (obj sprite))
  (check-type new-value (integer 1 1024)))
(defmethod (setf sprite-ncols) :before (new-value (obj sprite))
  (check-type new-value (integer 1 1024)))
(defmethod (setf sprite-flipx) :before (new-value (obj sprite))
  (check-type new-value boolean))
(defmethod (setf sprite-flipy) :before (new-value (obj sprite))
  (check-type new-value boolean))

(defmethod (setf sprite-index) (new-value (obj sprite))
  (with-slots (index ncols nrows) obj
    (setf index (mod new-value (* nrows ncols)))))

(defmethod (setf sprite-scale) :after (new-value (obj sprite))
  (setf (uploadp obj) T))
(defmethod (setf sprite-index) :after (new-value (obj sprite))
  (setf (uploadp obj) T))
(defmethod (setf sprite-nrows) :after (new-value (obj sprite))
  (setf (uploadp obj) T))
(defmethod (setf sprite-ncols) :after (new-value (obj sprite))
  (setf (uploadp obj) T))
(defmethod (setf sprite-flipx) :after (new-value (obj sprite))
  (setf (uploadp obj) T))
(defmethod (setf sprite-flipy) :after (new-value (obj sprite))
  (setf (uploadp obj) T))

(defmethod initialize-instance
    :after ((obj sprite) &key isam nrows ncols)
  (with-slots (ubo fbo sam tex) obj
    (let* ((itex (sampler-texture isam))
           (idim (texture-base-dimensions itex))
           (x    (floor (/ (first  idim) ncols)))
           (y    (floor (/ (second idim) nrows))))
      (setf tex (make-texture
                 NIL
                 :dimensions `(,x ,y)
                 :element-type (texture-element-type itex)))
      (setf fbo (make-fbo `(0 ,tex)))
      (setf sam (sample tex))
      (setf ubo (make-ubo NIL 'sprite-data))))
  (when (not (slot-boundp obj 'blend))
    (setf (slot-value obj 'blend) (make-blending-params))))

;; Code from cbaggers/daft
(defun-g calc-uv-mod ((ncols :int) (nrows :int) (index :int))
  (let* ((uv-scale  (v! (/ 1f0 ncols)
                        (/ 1f0 nrows)))
         (uv-offset (v! (* (mod index ncols)
                           (x uv-scale))
                        (* (floor (/ index ncols))
                           (y uv-scale)))))
    (values uv-scale uv-offset)))

(defun-g sprite-vert ((pos :vec2) &uniform (ubo sprite-data :ubo))
  (with-slots (ncols nrows scale index flipx flipy) ubo
    (multiple-value-bind (uv-scale uv-offset)
        (calc-uv-mod ncols nrows index)
      (let ((newpos (v! (* flipx (x pos))
                        (* flipy (y pos)))))
        (values (v! (* scale pos) 0 1)
                (s~ (+ .5 (* .5 newpos)) :xy)
                uv-scale
                uv-offset)))))

(defun-g sprite-frag ((uv        :vec2)
                      (uv-scale  :vec2)
                      (uv-offset :vec2)
                      &uniform
                      (sam :sampler-2d))
  (let ((uv (+ (* uv uv-scale) uv-offset)))
    (texture sam uv)))

(defpipeline-g sprite-pipe ()
  :vertex   (sprite-vert :vec2)
  :fragment (sprite-frag :vec2 :vec2 :vec2))

(defmethod upload :after ((obj sprite))
  (with-slots (nrows ncols index flipx flipy scale ubo isam blend fbo)
      obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (sprite-data-nrows (aref-c c 0)) nrows)
      (setf (sprite-data-ncols (aref-c c 0)) ncols)
      (setf (sprite-data-index (aref-c c 0)) index)
      (setf (sprite-data-scale (aref-c c 0)) scale)
      (setf (sprite-data-flipx (aref-c c 0)) (if flipx -1 +1))
      (setf (sprite-data-flipy (aref-c c 0)) (if flipy -1 +1)))
    (with-blending blend); ?????
    (with-setf (depth-mask) NIL
      (clear-fbo fbo)
      (map-g-into fbo #'sprite-pipe (get-quad-stream-v2)
                  :ubo ubo
                  :sam isam))))

(defclass sprite-2d (sprite)
  ((pos :initarg :pos))
  (:default-initargs
   :pos (v! 0 0)))

(defclass sprite-3d (sprite)
  ((pos :initarg :pos))
  (:default-initargs
   :pos (v! 0 0 0)))
