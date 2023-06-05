(in-package #:scenic)

(defclass cube (actor)
  ((sam :reader sam :initarg :sam :documentation "cubemap sampler"))
  (:default-initargs
   :buf (box)
   :shadowp NIL)
  (:documentation
   "abstract class, texture and sampler is generated elsewhere"))

(defun cube-p (obj)
  (typep obj 'cube))

(defun-g cube-vert ((g-pnt g-pnt)
                    &uniform
                    (view :mat4)
                    (proj :mat4))
  (let* ((pos3  (pos g-pnt))
         (pos4  (v! pos3 1))
         (cpos4 (* proj view pos4)))
    (values (s~ cpos4 :xyww)
            pos3)))

(defun-g cube-frag ((tc    :vec3)
                    &uniform
                    (color :vec3)
                    (sam   :sampler-cube))
  (let ((color3 (pow (s~ (texture sam tc) :xyz)
                     (vec3 2.2))))
    (values (v! (* color color3) 1))))

(defpipeline-g cube-pipe ()
  (cube-vert g-pnt)
  (cube-frag :vec3))

(defmethod paint :around (scene camera (obj cube) time)
  (with-setf* ((depth-test-function) #'<=
               (cull-face) :front
               (depth-mask) NIL)
    (call-next-method)))

(defun-g capture-cube-frag
    ((frag-pos  :vec4)
     (frag-norm :vec3) ;; keeping it to reuse other shaders
     &uniform
     (cube-sam  :sampler-cube))
  (v! (s~ (texture cube-sam (s~ frag-pos :xyz))
          :xyz)
      1))

(defpipeline-g capture-cube-pipe ()
  :vertex   (capture-vert g-pnt)
  :geometry (capture-geom (:vec3 3))
  :fragment (capture-cube-frag :vec4 :vec3))

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
