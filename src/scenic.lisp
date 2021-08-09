(in-package #:scenic)

(defun init ()
  (init-state
   (list (make-scene (list (make-pers))
                     (list (make-directional))))))

(def-simple-main-loop play-render (:on-start #'init)
  (let ((scene (current-scene)))
    ;; Draw Lights
    (dolist (l (lights scene))
      (dolist (a (actors scene))
        (draw a l 0f0)))
    ;; Draw actors
    (dolist (a (actors scene))
      (draw a (current-camera) 0f0)))
  (as-frame
    (progn)))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))


