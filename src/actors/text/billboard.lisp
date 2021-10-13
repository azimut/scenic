(in-package #:scenic)

;; TODO: separate generic billboarding
(defclass billboard (actor font renderable)
  ((dtf   :reader   dtf   :initarg :dtf)
   (fudge :accessor fudge :initarg :fudge))
  (:default-initargs
   :fudge (v! -.4 0)
   :dtf #'<
   :shadowp NIL
   :texture-opts '((0 :element-type :r8))
   :sample-opts '((:wrap :clamp-to-border))
   :dim '(256 32))
  (:documentation "3D in world camera facing billboard"))

(defun make-billboard (&rest args)
  (apply #'make-instance 'billboard args))

(defmethod initialize-instance :after ((obj billboard) &key msg)
  (setf (msg obj) msg))

(defmethod (setf msg) :after (new-value (obj billboard))
  (with-slots (fbo dim color fond-font fond-text) obj
    (clear-fbo fbo)
    (with-fbo-bound (fbo)
      (cepl.fond:fond-draw-simple
       fond-text
       (v! (- (* .5 (first (dim obj)))) ;; HORIZONTAL
           0 ;; VERTICAL
           (dim obj))))))

;; http://www.geeks3d.com/20140807/billboarding-vertex-shader-glsl/
(defun-g text-billboard-vert ((pos         :vec3)
                              &uniform
                              (scale       :float)
                              (fudge       :vec2)
                              (resolution  :vec2)
                              (model-view  :mat4)
                              (view-clip   :mat4))
  (let ((mv model-view)
        (uv (+ .5 (* .5 (v! (x pos) (y pos))))))
    (setf (aref mv 0 0) 1f0 (aref mv 0 1) 0f0 (aref mv 0 2) 0f0
          (aref mv 1 0) 0f0 (aref mv 1 1) 1f0 (aref mv 1 2) 0f0
          (aref mv 2 0) 0f0 (aref mv 2 1) 0f0 (aref mv 2 2) 1f0)
    (values (* view-clip (* mv (v! (* scale pos) 1)))
            (+ (v! (x uv)
                   (+ (- (* .5 (/ (x resolution) (y resolution))))
                      (* (/ (x resolution) (y resolution))
                         (y uv))))
               fudge))))

(defun-g text-billboard-frag ((uv :vec2) &uniform (color :vec3) (sam :sampler-2d))
  (let ((intensity  (x (texture sam uv))))
    (v! (* color intensity)
        intensity)))

(defpipeline-g text-billboard-pipe ()
  (text-billboard-vert :vec3)
  (text-billboard-frag :vec2))

(defmethod paint (scene (obj billboard) camera time)
  (with-slots (sam color scale blend dtf) obj
    (with-blending blend
      (with-setf* ((depth-mask) nil
                   (depth-test-function) dtf)
        (map-g #'text-billboard-pipe (get-quad-stream-v3)
               :sam (first sam)
               :scale scale
               :color color
               :fudge (fudge obj)
               :resolution (res obj)
               :model-view (model->view obj camera)
               :view-clip  (projection camera))))))
