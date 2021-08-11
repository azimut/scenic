(in-package #:scenic)

(defgeneric free (obj))
(defmethod free (obj) t)
(defmethod free ((obj null)) t)
(defmethod free ((obj list))
  (mapc #'free obj))

(defun slynk-hook ()
  #+slynk
  (slynk-mrepl::send-prompt (find (bt:current-thread) (slynk::channels)
                                  :key #'slynk::channel-thread)))

(defun init ()
  (free *state*)
  (slynk-hook)
  (init-state
   (list
    (make-scene
     (list (make-perspective
            :pos (v! 2 2 2)
            :rot (q:look-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
     (list (make-directional
            :pos (v! 100 100 100)))
     (make-simple-postprocess))))
  (push (make-instance 'actor :pos (v! 0 0 0))
        (actors (current-scene))))

(def-simple-main-loop play-render (:on-start #'init)
  (let* ((scene  (current-scene))
         (actors (actors scene))
         (camera (active-camera scene))
         (lights (lights scene))
         (time 0f0)
         (dt 0f0))
    #+nil
    (dolist (l lights)
      (dolist (a actors)
        (draw a l time)))
    (draw scene camera time)
    (update scene dt)
    (as-frame
      (draw (post scene) camera time))))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))

