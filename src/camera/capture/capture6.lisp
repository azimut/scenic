(in-package #:scenic)

;; Independent of CAPTURE due on CEPL a texture cannot be manipulated
;; by 2 fbos at the same time. (~Hipothesis~)
;;
;; For when it gets hard to modify a regular pipeline to fit into a single pass capture

(defclass capture6 (pers)
  ((tex :reader tex :documentation "cube texture")
   (sam :reader sam :documentation "sampler of the cube")
   (fbo :reader fbo :documentation "temporal fbo of a side of the cube")
   (dim :reader dim :initarg :dim :documentation "dimensions")
   (rotations :documentation "list of quaternions, camera rotations"))
  (:default-initargs
   :fov 90f0
   :dim '(128 128))
  (:documentation "captures a scene in 6 passes"))

(defmethod free :after ((obj capture6))
  (free (tex obj))
  (free (fbo obj)))

(defmethod initialize-instance :before ((obj capture6) &key dim)
  (assert (length= dim 2)))
(defmethod initialize-instance :after ((obj capture6) &key)
  (with-slots (fbo tex sam rotations dim) obj
    (setf tex (make-texture NIL :cubes T :dimensions dim :element-type :rgb16f))
    (setf sam (sample tex :wrap           :clamp-to-edge
                          :minify-filter  :linear-mipmap-linear
                          :magnify-filter :linear))
    (setf fbo (make-fbo `(0 :dimensions ,dim)))
    (setf rotations
          (list (q:look-at (v! 0 -1 0) (v! 0 0 0) *vec3-right*)
                (q:look-at (v! 0 -1 0) (v! 0 0 0) *vec3-left*)
                (q:look-at (v! 0 -1 0) (v! 0 0 0) *vec3-down*)
                (q:look-at (v! 0 -1 0) (v! 0 0 0) *vec3-up*)
                (q:look-at (v! 0 -1 0) (v! 0 0 0) *vec3-forward*)
                (q:look-at (v! 0 -1 0) (v! 0 0 0) *vec3-back*)))))

#+nil
(defmethod paint :around (scene (camera capture6) actor time)
  (with-slots (fbo rotations) camera
    (with-setf* ((depth-test-function) #'<=
                 (cull-face) :front
                 (depth-mask) NIL)
      (loop :for qrotation :in rotations
            :for cube-face :from 0
            :do (setf (rot camera) qrotation)
                (setf (attachment fbo 0)
                      (texref (tex camera) :cube-face cube-face))
                (with-fbo-bound (fbo)
                  (call-next-method))))))
