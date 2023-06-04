(in-package #:scenic)

(defclass cube (actor)
  ((tex  :reader tex  :documentation "cubemap texture" :initarg :tex)
   (sam  :reader sam  :documentation "cubemap sampler"))
  (:default-initargs
   :buf (box)
   :shadowp NIL)
  (:documentation "textured RGB8 cubemap, from 6 images"))

(defmethod print-object ((obj cube) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos) obj
      (format stream "(~a ~a ~a)" (x pos) (y pos) (z pos)))))

(defmethod free ((obj cube))
  (free (tex obj)))

(defun-g cube-vert ((g-pnt g-pnt) &uniform (view :mat4) (proj :mat4))
  (let* ((pos3  (pos g-pnt))
         (pos4  (v! pos3 1))
         (cpos4 (* proj view pos4)))
    (values (s~ cpos4 :xyww)
            pos3)))

(defun-g cube-frag ((tc    :vec3) &uniform
                    (color :vec3)
                    (sam   :sampler-cube))
  (let ((color3 (s~ (texture sam tc) :xyz)))
    (values (v! (* color color3) 1))))

(defpipeline-g cube-pipe ()
  (cube-vert g-pnt)
  (cube-frag :vec3))

(defmethod paint :around (scene (obj cube) camera time)
  (with-setf* ((depth-test-function) #'<=
               (cull-face) :front
               (depth-mask) NIL)
    (call-next-method)))

(defmethod paint (scene (obj cube) camera time)
  (with-slots (buf sam color) obj
    (map-g #'cube-pipe buf
           :sam sam
           :color color
           :view (q:to-mat4 (q:inverse (rot camera)))
           :proj (projection camera))))

;; TODO: texture caching
(defun make-cube-tex (&rest paths)
  "Returns a gpu texture FROM the provided images"
  (assert (= 6 (length paths)))
  (log4cl:log-info "loading CUBEMAP texture")
  (with-c-arrays-freed
      (ca (mapcar
           (lambda (p)
             (dirt:load-image-to-c-array
              (asdf:system-relative-pathname :scenic p)))
           paths))
    (make-texture ca :element-type :rgb8 :cubes t)))

(defmethod initialize-instance :after ((obj cube) &key tex)
  (setf (slot-value obj 'sam)
        (sample tex :wrap :clamp-to-edge :magnify-filter :linear)))

(defun make-cube (left right up down front back &rest args)
  (let ((tex (make-cube-tex left right up down front back)))
    (apply #'make-instance 'cube :tex tex args)))

(defun cube-p (obj)
  (typep obj 'cube))
