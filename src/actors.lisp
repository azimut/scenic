(in-package #:scenic)

(defclass actor ()
  ((pos   :initarg :pos   :accessor pos   :documentation "3d position")
   (rot   :initarg :rot   :accessor rot   :documentation "3d rotation")
   (buf   :initarg :buf   :accessor buf   :documentation "buffer stream")
   (color :initarg :color :accessor color :documentation "base color")
   (scale :initarg :scale :accessor scale :documentation "vextex fudge scale"))
  (:default-initargs
   :color (v! 1 1 1)
   :pos (v! 0 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box))
  (:documentation "base object"))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

(defun make-actor (&key (w 1f0) (h 1f0) (d 1f0) (pos (v! 0 0 0)))
  (make-instance 'actor :buf (box w h d) :pos pos))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))

(defun-g actor-vert ((vert g-pnt) &uniform
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4)
                     (scale       :float)
                     (dirlights   dir-light-data   :ubo)
                     (light-clip-mat :mat4)
                     (light-clip :mat4)
                     (light-world :mat4))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (light-pos  (vector (v! 0 0 0 0)
                             (v! 0 0 0 0)
                             (v! 0 0 0 0))))
    (dotimes (i (size dirlights))
      (setf (aref light-pos i)
            (* (aref (lightspace dirlights) i) world-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz)
            light-pos
            (* (aref (lightspace dirlights) 0) world-pos)
            ;;(* light-clip light-world world-pos)
            ;;(* light-clip-mat world-pos)
            )))

(defun-g actor-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                     (light-pos (:vec4 3))
                     (light-clip-pos :vec4)
                     &uniform
                     (time        :float)
                     (color       :vec3)
                     (shadow      :sampler-2d)
                     (shadows     :sampler-2d-array)
                     (dirlights   dir-light-data   :ubo)
                     (pointlights point-light-data :ubo))
  (let ((final-color (v! 0 0 0)))
    ;;(setf final-color (* color (shadow-factor shadow light-clip-pos .003)))
    ;;#+nil
    (dotimes (i (size dirlights))
      (incf final-color
            (* (dir-light-apply
                color
                (aref (colors dirlights) i)
                (aref (positions dirlights) i)
                frag-pos
                frag-norm)
               ;;(shadow-factor shadows light-clip-pos .003 i)
               (shadow-factor shadows (aref light-pos i) .003 i)
               ;;(shadow-factor shadow light-clip-pos .003)
               )))
    #+nil
    (dotimes (i 0;(size pointlights)
                )
      (with-slots (colors positions linear quadratic) pointlights
        (incf final-color (point-light-apply
                           color
                           (aref colors i)
                           (aref positions i)
                           frag-pos
                           frag-norm
                           (aref linear i)
                           (aref quadratic i)))))
    ;;(incf final-color (v! .01 .01 .01))
    (v! final-color 1)))

(defpipeline-g actor-pipe ()
  :vertex (actor-vert g-pnt)
  :fragment (actor-frag :vec2 :vec3 :vec3
                        (:vec4 3)
                        :vec4))

(let ((stepper (make-stepper (seconds 5) (seconds 5))))
  (defmethod update ((actor actor) dt)
    #+nil
    (when (funcall stepper)
      (setf (rot actor)
            (q:* (q:from-axis-angle (v! 1 0 0) (radians (random 90)))
                 (q:from-axis-angle (v! 0 0 1) (radians (random 90)))))
      (setf (pos actor) (v! (+ -2.5 (* 5 (sin (mynow)))) 0
                            (+ -2.5 (* 5 (cos (mynow)))) 0)))))

(defmethod draw ((actor actor) (camera renderable) time)
  (let* ((scene (current-scene))
         (light-camera (first (dir-lights (lights scene)))))
    (with-slots (buf scale color) actor
      (map-g #'actor-pipe buf
             :shadows (dir-sam (lights scene))
             :shadow *shadow-sam*
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :light-clip (projection light-camera)
             :light-world (world->view light-camera)
             :light-clip-mat (world->clip light-camera)
             :dirlights (dir-ubo (lights scene))
             :pointlights (point-ubo (lights scene))
             :time time))))

(defun-g shadow-factor ((light-sampler      :sampler-2d-array)
                        (pos-in-light-space :vec4)
                        (bias               :float)
                        (light-index        :uint))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (current-depth (z proj-coords))
         (closest-depth (x (texture light-sampler (v! (s~ proj-coords :xy) light-index))))
         (shadow        (step (- current-depth bias) closest-depth)))
    (if (> current-depth 1)
        1f0
        shadow)))
(defun-g shadow-factor ((light-sampler      :sampler-2d-array)
                        (pos-in-light-space :vec4)
                        (bias               :float)
                        (index              :uint))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (texel-size (/ (vec2 1f0)
                        1028f0;(x (texture-size light-sampler 0))
                        ))
         (uv (s~ proj-coords :xy)))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x -1) (<= x 1) (++ x)
             (for (y -1) (<= y 1) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                (v! uv+offset index)))))
                    (incf shadow (step pcf-depth (- our-depth bias)))))))
    ;;
    (- 1 (/ shadow 9f0))))
(defun-g shadow-factor ((light-sampler      :sampler-2d)
                        (pos-in-light-space :vec4)
                        (bias               :float))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (current-depth (z proj-coords))
         (closest-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (shadow        (step (- current-depth bias) closest-depth)))
    (if (> current-depth 1)
        1f0
        shadow)))


;; BIAS static - PCF
(defun-g shadow-factor ((light-sampler      :sampler-2d)
                        (pos-in-light-space :vec4)
                        (bias               :float))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (num-samples 3f0)
         (num-samples-start (/ (1- num-samples) 2))
         (shadow 0f0)
         (texel-size (/ 1f0
                        (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x num-samples-start) (<= x num-samples) (++ x)
             (for (y num-samples-start) (<= y num-samples) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                uv+offset))))
                    (incf shadow (if (> (- our-depth bias) pcf-depth)
                                     1f0
                                     0f0))))))
    ;;
    (- 1 (/ shadow (* num-samples num-samples)))))

(defun-g shadow-factor ((light-sampler      :sampler-2d)
                        (pos-in-light-space :vec4)
                        (bias               :float))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (texel-size (/ (vec2 1f0) (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x -1) (<= x 1) (++ x)
             (for (y -1) (<= y 1) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                uv+offset))))
                    (incf shadow (step pcf-depth (- our-depth bias)))))))
    ;;
    (- 1 (/ shadow 9f0))))
