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
            :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
     (make-lights
      :collection
      (list (make-directional
             :pos (v! 100 100 100)
             :rot (q:point-at (v! 0 1 0) (v! 100 100 100) (v! 0 0 0)))))
     (make-simple-postprocess))))
  (push (make-instance 'actor :pos (v! 0 0 0))
        (actors (current-scene))))

(def-simple-main-loop play-render (:on-start #'init)
  (let* ((scene  (current-scene))
         (actors (actors scene))
         (camera (active-camera scene))
         (lights (collection (lights scene)))
         (time 0f0)
         (dt 0f0))
    ;;#+nil
    ;; (dolist (l lights)
    ;;   (dolist (a actors)
    ;;     (draw a l time)))
    (draw scene camera time)
    (update scene dt)
    (as-frame
      ;;(draw *tmpmulti* camera time)
      (draw (post scene) camera time))))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))

(defun-g multi-frag ((uv :vec2) &uniform (sams :sampler-2d-array))
  (v! (* 1000 (x (texture sams (v! uv 0))))
      0
      0
      1))

(defpipeline-g multi-pipe (:points)
  :fragment (multi-frag :vec2))

(defclass multitext (postprocess)
  ())

(defmethod draw ((obj multitext) camera time)
  (with-slots (bs) obj
    (map-g #'multi-pipe bs)))
