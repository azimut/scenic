(in-package #:scenic)

(defclass audio-sfx (physic-box emitter nepal::sfx)
  ()
  (:default-initargs
   :occludesp nil))

(defmethod free :after ((obj audio-sfx))
  (nepal::delete-source (nepal::audio-name obj)))
