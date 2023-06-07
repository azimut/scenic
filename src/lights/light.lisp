(in-package #:scenic)

(defclass light (listener drawable)
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
   :interval 0.01 ; multiple actors might be moving, but we want just one light update
   :uploadp T
   :shadowp NIL
   :fudge 0.03
   :scale 1f0
   :debugp NIL
   :buf (sphere)
   :color (v! 1 1 1))
  (:documentation "base class for all lights"))

(defmethod (setf pos)   :after (_ (obj light)) (setf (uploadp obj) T));; from children!
(defmethod (setf color) :after (_ (obj light)) (setf (uploadp obj) T))
(defmethod (setf fudge) :after (_ (obj light)) (setf (uploadp obj) T))

(defmethod upload :around ((light light))
  (when (uploadp light)
    (call-next-method)
    (setf (uploadp light) NIL)))

(defmethod draw :around (scene (obj light) time)
  (call-next-method)
  (setf (drawp obj) NIL))

(defmethod paint :around (scene (light light) (actor drawable) _)
  (when (shadowp actor)
    (call-next-method)))

(defmethod handle :after ((event movement) (obj light))
  (setf (uploadp obj) T)
  (setf (drawp   obj) T))

(defun-g light-vert ((vert g-pnt) &uniform (model-clip :mat4) (scale :float))
  (let ((pos (* scale (pos vert))))
    (* model-clip (v! pos 1))))

(defun-g light-frag (&uniform (color :vec3))
  (v! (* color 100) 1)) ;; NOTE: blow it to cancel shading

(defpipeline-g light-pipe ()
  :vertex (light-vert g-pnt)
  :fragment (light-frag))

(defmethod paint (scene (camera renderable) (light light) _)
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
