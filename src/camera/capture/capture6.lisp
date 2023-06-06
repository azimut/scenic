(in-package #:scenic)

(defclass capture6 (capture)
  ((tmpfbo :initform nil :documentation "fbo used to render a face of the cubemap")
   (rotations :documentation "camera rotations"))
  (:documentation "captures a scene in 6 passes"))

(defmethod free :after ((obj capture6))
  (free (slot-value obj 'tmpfbo)))

(defmethod initialize-instance :after ((obj capture6) &key)
  (setf (slot-value obj 'rotations)
        (list (q:look-at (v! 0 1 0) (v! 0 0 0) *vec3-left*)
              (q:look-at (v! 0 1 0) (v! 0 0 0) *vec3-right*)
              (q:look-at (v! 0 1 0) (v! 0 0 0) *vec3-up*)
              (q:look-at (v! 0 1 0) (v! 0 0 0) *vec3-down*)
              (q:look-at (v! 0 1 0) (v! 0 0 0) *vec3-back*)
              (q:look-at (v! 0 1 0) (v! 0 0 0) *vec3-forward*))))

#+nil
(defmethod paint :around (scene (camera capture6) actor time)
  (with-slots (tmpfbo rotations) camera
    ;; Here due RENDERABLE initialize-instance happens :after this one
    (unless tmpfbo
      (setf tmpfbo (make-fbo `(0 ,(texref (first (tex camera)) :cube-face 0)))))
    (with-setf* ((depth-test-function) #'<=
                 (cull-face) :front
                 (depth-mask) NIL)
      (loop :for qrotation :in rotations
            :for cube-face :from 0
            :do (setf (rot camera) qrotation)
                (setf (attachment tmpfbo 0)
                      (texref (first (tex camera)) :cube-face cube-face))
                (with-fbo-bound (tmpfbo)
                  (call-next-method))))))
