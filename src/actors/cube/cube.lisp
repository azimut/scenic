(in-package #:scenic)

(defclass cube (actor)
  ((sam :reader sam :initarg :sam :documentation "cubemap sampler"))
  (:default-initargs
   :buf (box)
   :shadowp NIL)
  (:documentation
   "abstract class, texture and sampler is generated elsewhere"))

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

(defmethod paint :around (scene camera (obj cube) time)
  (with-setf* ((depth-test-function) #'<=
               (cull-face) :front
               (depth-mask) NIL)
    (call-next-method)))

;; Used for IBL or other(? captures
(defmethod paint (scene (camera capture) (actor cube) time)
  (with-setf* ((depth-test-function) #'<=
               (cull-face) :front
               (depth-mask) NIL)
    (with-slots (buf) actor
      (map-g #'capture-cube-pipe buf
             :world (m4:identity)      ;; ??
             :projections (ubo camera) ;; ??
             :cube-sam (sam actor)))))

(defun cube-p (obj)
  (typep obj 'cube))
