(in-package #:scenic)

(defclass raycast (uploadable drawable)
  ((space :reader   space :documentation "ODE space")
   (color :accessor color :initarg :color)
   (from  :accessor from  :initarg :from)
   (to    :accessor to    :initarg :to
          :documentation "our destination, in ODE it's an infinite line")
   (ray   :reader   ray :documentation "ODE ray")
   (hit   :reader   hit :documentation "ODE hitpoints")
   (gar   :reader   gar :documentation "gpu array with from&to positions")
   (buf   :reader   buf :documentation ":lines bufferstream of gpu array above"))
  (:default-initargs
   :from (v! 0 2 0)
   :to   (v! 0 0 0)
   :shadowp NIL
   :color (v! 0 1 0))
  (:documentation "ode raycast, you wouldn't use this directly"))

(defun make-raycast (&rest args)
  (apply #'make-instance 'raycast args))

(defmethod print-object ((obj raycast) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (from to) obj
      (format stream "(~,2f ~,2f ~,2f)->(~,2f ~,2f ~,2f)"
              (x from) (y from) (z from) (x to) (y to) (z to)))))

(defmethod initialize-instance :before ((obj raycast) &key from to)
  (assert (not (v3:= from to))))
(defmethod initialize-instance :after ((obj raycast) &key)
  (with-slots (hit ray gar buf space) obj
    (setf space (space (current-scene)))
    (setf hit (cffi:foreign-alloc '%ode:real :count 4))
    (setf ray (%ode:create-ray space 0f0))
    (%ode:geom-set-category-bits ray (getf *ode-bits* :ray))
    (%ode:geom-set-collide-bits ray (logand #xffffffff (lognot (getf *ode-bits* :camera))))
    (setf gar (make-gpu-array nil :element-type :vec3 :dimensions 2))
    (setf buf (make-buffer-stream gar :primitive :lines)))
  (upload obj))

(defmethod upload ((obj raycast))
  (with-slots (from to ray gar drawp) obj
    (let ((dir (v3:normalize (v3:- to from))))
      (%ode:geom-ray-set ray (x from) (y from) (z from) (x dir) (y dir) (z dir))
      (%ode:geom-ray-set-length ray (v3:length (v3:- to from))))
    #+nil
    (when drawp
      (with-gpu-array-as-c-array (carr gar)
        (setf (aref-c carr 0) (v! (x from) (y from) (z from)))
        (setf (aref-c carr 1) (v! (x   to) (y   to) (z   to)))))))

(defmethod (setf drawp) :after (_ (obj raycast)) (setf (uploadp obj) T))
(defmethod (setf from)  :after (_ (obj raycast)) (setf (uploadp obj) T))
(defmethod (setf to)    :after (_ (obj raycast)) (setf (uploadp obj) T))

(defmethod free ((obj raycast))
  (%ode:geom-destroy (ray obj))
  (cffi:foreign-free (hit obj))
  (free (gar obj)))

(cffi:defcallback ray-callback
    :void ((data :pointer) (g1 %ode:geom-id) (g2 %ode:geom-id))
  (cffi-c-ref:c-let ((hit-position %ode:real :count 4 :from data))
    (cffi-c-ref:c-with ((contacts %ode:contact :count 32))
      (let ((ncontacts (%ode:collide
                        g1 g2 32
                        (contacts :geom &)
                        (cffi:foreign-type-size '%ode:contact))))
        (dotimes (i ncontacts)
          (when (< (contacts i :geom :depth)
                   (hit-position 3))
            (when (and (= i 0) ;; last contact is the closest?
                       (/= ode:+infinity+ (contacts i :geom :depth)))
              (if (cffi:null-pointer-p (%ode:geom-get-body g2))
                  (hit-floor)
                  (hit-object (pointer-to-actor (%ode:geom-get-body g2)))))
            (setf (hit-position 0) (cffi:mem-ref (contacts i :geom :pos) :float 0))
            (setf (hit-position 1) (cffi:mem-ref (contacts i :geom :pos) :float 1))
            (setf (hit-position 2) (cffi:mem-ref (contacts i :geom :pos) :float 2))
            (setf (hit-position 3) (contacts i :geom :depth))))))))

(defun hit-p (raycast)
  "returns a v4, of the place where the ray hitted something
   :xyz are the 3D coordinates
   :w is the distance to the hitpoint, might be infinity"
  (cffi-c-ref:c-let ((hit-position %ode:real :count 4 :from (hit raycast)))
    (setf (hit-position 3) ode:+infinity+)
    (%ode:space-collide2 (ray raycast)
                         (space raycast)
                         (hit-position &)
                         (cffi:callback ray-callback))
    (list :contact (v! (hit-position 0) (hit-position 1) (hit-position 2))
          :distance (hit-position 3)
          :hit-p (/= (hit-position 3) ode:+infinity+))))

(defmethod hit-object (obj)
  (print "hit fallthrough"))
(defun hit-floor ()
  (print "hit floor"))

(defun-g line-vert ((vert :vec3) &uniform (model-clip :mat4))
  (* model-clip (v! vert 1)))
(defun-g line-frag (&uniform (color :vec3))
  (v! color 1))
(defpipeline-g line-pipe (:lines)
  (line-vert :vec3)
  (line-frag))

(defmethod paint :around (scene camera (obj raycast) time)
  #+nil
  (when (drawp obj)
    (call-next-method)))
(defmethod paint (scene camera (obj raycast) time)
  (with-slots (buf color from to) obj
    (map-g #'line-pipe buf
           :model-clip (world->clip camera)
           :color color)))

;; NOTE: usually not all actors upload...
(defmethod update ((obj raycast) time)
  (upload obj))

(defun test-occluded-p (ray from to)
  (setf (from ray) (pos from))
  (setf (to   ray) (pos to))
  (upload ray) ;; !!!
  (cffi-c-ref:c-with ((contacts %ode:contact :count 32))
    (let ((ncontacts (%ode:collide (ray ray) (space (current-scene)) 32
                                   (contacts :geom &)
                                   (cffi:foreign-type-size '%ode:contact))))
      (when (plusp ncontacts)
        (not (sb-sys:sap= (geom to)
                          (contacts (1- ncontacts) :geom :g2)))))))
