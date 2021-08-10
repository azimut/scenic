(in-package #:scenic)

(defgeneric free (obj))
(defmethod free (obj) t)
(defmethod free ((obj null)) t)
(defmethod free ((obj list))
  (mapc #'free obj))

(defun slynk-hook ()
  #+slynk
  (slynk-mrepl::send-prompt
   (find (bt:current-thread) (slynk::channels)
         :key #'slynk::channel-thread)))

(defun init ()
  (free *state*)
  (slynk-hook)
  (init-state
   (list
    (make-scene
     (list (make-perspective
            :pos (v! -4 0 -4)
            :rot (q:look-at (v! 0 1 0) (v! -4 0 -4) (v! 0 0 0))))
     (list (make-directional
            :pos (v! 100 0 100)))
     (make-simple-postprocess)))))

(def-simple-main-loop play-render (:on-start #'init)
  (let* ((scene  (current-scene))
         (actors (actors scene))
         (camera (active-camera scene))
         (lights (lights scene))
         (time 0f0))
    (dolist (l lights)
      (dolist (a actors)
        (draw a l time)))
    (dolist (a actors)
      (draw a camera time))
    (as-frame
      (draw (post scene) camera time))))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop)
  (free *state*))
