(in-package #:scenic)

(defvar *emitters* ())

(defclass emitter ()
  ())

(defmethod initialize-instance :after ((obj emitter) &key)
  (push obj *emitters*))

(defmethod free :after ((obj emitter))
  (delete obj *emitters*))

(defmethod update ((camera camera-audio-defered) dt)
  ;; (god-move .2 dt camera)
  ;; (full-rot .2 dt camera)
  (human-move .25 dt camera)
  (human-rot  .15 dt camera)
  )

(defmethod handle ((e tick) (camera camera-audio))
  (setf (from camera) (copy-seq (pos camera)))
  (dolist (emitter *emitters*)
    (when (nepal::playing-source-p (nepal::audio-source emitter))
      (if (occluded-p camera emitter)
          (setf (nepal::state-gain emitter) +0.50)
          (setf (nepal::state-gain emitter) +1.00)))))

(defmethod occluded-p ((camera camera-audio) (emitter emitter))
  (setf (to camera) (copy-seq (pos emitter)))
  (upload camera) ;; !!!
  (cffi-c-ref:c-with ((contacts %ode:contact :count 32))
    (let ((ncontacts (%ode:collide (ray camera) (space (current-scene)) 32
                                   (contacts :geom &)
                                   (cffi:foreign-type-size '%ode:contact))))
      (when (plusp ncontacts)
        (loop
          :with min-index := 0
          :with min-depth := ode:+infinity+
          :for i :from 0 :to (1- ncontacts)
          :for current-depth := (contacts i :geom :depth)
          :when (< current-depth min-depth)
            :do (setf min-depth current-depth
                      min-index i)
          :finally
             (return
               (not (sb-sys:sap= (geom emitter)
                                 (contacts min-index :geom :g2)))))))))
