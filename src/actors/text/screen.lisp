(in-package #:scenic)

(defclass screentext (font postprocess)
  ((pos   :accessor pos   :initarg :pos)
   (color :accessor color :initarg :color)
   (scale :accessor scale :initarg :scale))
  (:default-initargs
   :scale 2f0
   :pos (v! 0 0)
   :color (v! 1 1 1 1))
  (:documentation "2D on screentext text"))

(defmethod print-object ((obj screentext) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos path) obj
      (format stream "(~$ ~$) ~s" (x pos) (y pos) path))))

(defun make-screentext (&rest args)
  (apply #'make-instance 'screentext args))

(defmethod initialize-instance :after ((obj screentext) &key msg)
  (setf (msg obj) msg))

(defstruct-g fond-vertex
  (position     :vec2)
  (in-tex-coord :vec2))

(defun-g calc-text-uvs ((pos :vec2) (extent :vec4) (scale :float))
  (vec4 (* scale (/ (+ (x pos) (x extent))
                    (- (z extent) 1.0)))
        (* scale (/ (- (y pos) (y extent))
                    (+ (w extent) 1.0)))
        0.0
        1.0))

(defun-g tvert ((text-info fond-vertex) &uniform (extent :vec4) (scale :float))
  (with-slots (position in-tex-coord) text-info
    (values (calc-text-uvs position extent scale)
            in-tex-coord)))

(defun-g tfrag ((tex-coord :vec2) &uniform (tex-image :sampler-2d) (color :vec4))
  (let ((intensity (x (texture tex-image tex-coord))))
    (* (vec4 intensity) color)))

(defpipeline-g fond-pipeline ()
  (tvert fond-vertex)
  (tfrag :vec2))

(defmethod blit (scene (obj screentext) camera time)
  (with-slots (pos color blend scale fond-text fond-font) obj
    (with-blending blend
      (map-g #'fond-pipeline (cepl.fond::fond-text-stream fond-text)
             :tex-image (cepl.fond::fond-font-sampler fond-font)
             :extent (v! pos (dim camera))
             :scale scale
             :color color))))
