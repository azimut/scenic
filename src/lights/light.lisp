(in-package #:scenic)

(defclass light (listener)
  ((fbo     :reader fbo :documentation "light camera fbo")
   (idx     :reader idx :documentation "light index on texture, for the type")
   (ubo     :reader ubo :documentation "reference to scene ubo with light data")
   (buf     :reader buf :initarg :buf)
   (fudge   :accessor fudge :initarg :fudge)
   (scale   :accessor scale :initarg :scale)
   (debugp  :accessor debugp :initarg :debugp)
   (color   :initarg :color   :accessor color   :documentation "light color")
   (uploadp :initarg :uploadp :accessor uploadp :documentation "if TRUE, upload the information to the GPU"))
  (:default-initargs
   :uploadp T
   :fudge 0.03
   :scale 1f0
   :debugp NIL
   :buf (sphere)
   :color (v! 1 1 1))
  (:documentation "base class for all lights"))

(defmethod (setf pos)   :after (_ (obj light)) (setf (uploadp obj) T))
(defmethod (setf color) :after (_ (obj light)) (setf (uploadp obj) T))
(defmethod (setf fudge) :after (_ (obj light)) (setf (uploadp obj) T))

(defmethod upload :around ((light light))
  (when (uploadp light)
    (call-next-method)
    (setf (uploadp light) NIL)))

(defun-g light-vert ((vert g-pnt) &uniform (model-clip :mat4) (scale :float))
  (let* ((pos (* scale (pos vert)))
         (clip-pos (* model-clip (v! pos 1))))
    clip-pos))
(defun-g light-frag (&uniform (color :vec3))
  (v! color 1))
(defpipeline-g light-pipe ()
  (light-vert g-pnt)
  (light-frag))

(defmethod paint :around (scene (actor drawable) (light light) _)
  (when (shadowp actor)
    (call-next-method)))

(defmethod paint (scene (light light) (camera renderable) _)
  (with-slots (buf color scale debugp) light
    (when debugp
      (map-g #'light-pipe buf
             :model-clip (model->clip light camera)
             :scale scale
             :color color))))

(let ((tmp T))
  (defun debug-scene ()
    (mapcan (lambda (light) (setf (debugp light) tmp))
            (lights (current-scene)))
    (setf tmp (not tmp))))
