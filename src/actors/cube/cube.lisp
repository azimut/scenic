(in-package #:scenic)

(defclass cube (actor)
  ((sam   :reader   sam
          :initarg :sam
          :documentation "cubemap sampler")
   (gamma :accessor gamma
          :initarg :gamma
          :documentation "cubemap gamma correction")
   (delta :accessor delta
          :initarg :delta
          :documentation "cubemap gamma correction used when capturing"))
  (:default-initargs
   :buf (box)
   :shadowp NIL
   :delta 0.5
   :gamma 0.5)
  (:documentation
   "abstract class, texture and sampler is generated elsewhere"))

(defmethod initialize-instance :before ((obj cube) &key gamma delta)
  (check-type gamma single-float)
  (check-type delta single-float))

(defmethod (setf gamma) :before (new-value (obj cube))
  (check-type new-value single-float))
(defmethod (setf delta) :before (new-value (obj cube))
  (check-type new-value single-float))

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
                    (gamma :float)
                    (color :vec3)
                    (sam   :sampler-cube))
  (let* ((color3 (s~ (texture sam tc) :xyz))
         (color3 (pow (/ color3 (+ color3 1))
                      (vec3 (/ gamma)))))
    (v! (* color color3) 1)))

(defpipeline-g cube-pipe ()
  (cube-vert g-pnt)
  (cube-frag :vec3))

(defmethod paint :around (scene camera (obj cube) time)
  (with-setf* ((depth-test-function) #'<=
               (cull-face) :front
               (depth-mask) NIL)
    (call-next-method)))

;;------------------------------

(defun-g capture-cube-frag
    ((frag-pos  :vec3)
     (frag-norm :vec3) ;; keeping it to reuse other shaders
     &uniform
     (cube-sam  :sampler-cube)
     (color     :vec3)
     (gamma     :float))
  (let* ((color3 (s~ (texture cube-sam frag-pos) :xyz))
         (color3 (pow (/ color3 (+ color3 1))
                      (vec3 (/ gamma)))))
    (v! (* color color3) 1)))

(defpipeline-g capture-cube-pipe ()
  :vertex   (capture-vert g-pnt)
  :geometry (capture-geom (:vec3 3))
  :fragment (capture-cube-frag :vec3 :vec3))

;; Used for IBL or other(? captures
(defmethod paint (scene (camera capture) (actor cube) time)
  (with-slots (buf delta color) actor
    (map-g #'capture-cube-pipe buf
           :color color
           :gamma delta
           :world (m4:identity)      ;; ??
           :projections (ubo camera) ;; ??
           :cube-sam (sam actor))))
