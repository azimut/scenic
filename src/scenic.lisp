(in-package #:scenic)

(defmethod free ((obj null)))
(defmethod free ((obj list))
  (mapc #'free obj))

(defun init ()
  #+slynk
  (slynk-mrepl::send-prompt
   (find (bt:current-thread) (slynk::channels)
         :key #'slynk::channel-thread))
  (free *state*)
  (init-state
   (list
    (make-scene
     (list (make-pers
            :pos (v! -4 0 -4)
            :rot (q:look-at (v! 0 1 0) (v! -4 0 -4) (v! 0 0 0))))
     (list (make-directional
            :pos (v! 100 0 100)))
     (make-simple-postprocess)))))

(def-simple-main-loop play-render (:on-start #'init)
  (let ((scene (current-scene)))
    ;; Draw Lights Shadows
    (dolist (l (lights scene))
      (dolist (a (actors scene))
        (draw a l 0f0)))
    ;; Draw actors
    (dolist (a (actors scene))
      (draw a (current-camera) 0f0))
    ;; Post
    (as-frame
      (post (postprocess scene)))))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop)
  (free *state*))

