(in-package #:scenic)

(defclass simple (postprocess)
  ((exposure :initarg :exposure
             :initform (error ":exposure must be specified")
             :accessor exposure
             :documentation "camera exposure")))

(defun make-simple-postprocess (&key (exposure 1f0))
  (make-instance 'simple :exposure exposure))

;; https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
(defun-g linear-to-srgb ((c :vec3))
  (let ((igamma #.(/ 1f0 2.2f0)))
    (pow c (vec3 igamma))))

(defun-g tone-map-acesfilm ((x :vec3) (exposure :float))
  (let ((x (* exposure x))
        (a 2.51)
        (b 0.03)
        (c 2.43)
        (d 0.59)
        (e 0.14))
    (linear-to-srgb
     (clamp (/ (* x (+ b (* a x)))
               (+ e (* x (+ d (* c x)))))
            0f0 1f0))))

(defun-g frag-2d ((uv :vec2) &uniform (sam :sampler-2d) (exposure :float))
  (let* ((final-color (s~ (texture sam uv) :xyz))
         (ldr  (tone-map-acesfilm final-color exposure))
         (luma (rgb->luma-bt601 ldr)))
    (v! ldr luma)
    ;;(tone-map-reinhard final-color exposure)
    ))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))

(defmethod draw :around ((obj simple) camera time)
  (with-setf* ((depth-test-function) nil
               (depth-mask)          nil
               (cull-face)           nil)
    (call-next-method)))

(defmethod draw ((obj simple) camera time)
  (with-slots (exposure bs) obj
    (map-g #'generic-2d-pipe bs
           :sam (first (sam camera))
           :exposure exposure)))
