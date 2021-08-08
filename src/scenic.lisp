(in-package #:scenic)

(defun init ()
  )

(defclass scene ()
  (cameras
   lights)
  (:documentation "scene object"))



(def-simple-main-loop play-render (:on-start #'init)
  (as-frame
    (progn)))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))


