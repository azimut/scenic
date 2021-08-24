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
  (reset-material-counter)
  (skitter-cleanup)
  (init-all-the-things)
  (skitter:listen-to #'window-listener-trampoline (skitter:window 0) :size)
  (setf (last-time *state*) (get-internal-real-time)))

(def-simple-main-loop play-render (:on-start #'init)
  (let* ((scene  (current-scene))
         (camera (active-camera scene))
         (now    (get-internal-real-time))
         (dt     (* (- now (last-time *state*) .001)))
         (dt     (if (> dt .16) .00001 dt)))
    (setf (last-time *state*) now)
    (upload scene)
    (update scene dt)

    (dolist (l (remove-if #'point-p (lights scene)))
      (draw scene l dt))

    (when-let ((points (remove-if-not #'point-p (lights scene))))
      (let ((1-point (first points)))
        (when (drawp 1-point)
          (clear-fbo (fbo 1-point) :d)
          (dolist (p points)
            (draw scene p dt))
          (setf (drawp (first points)) NIL))))

    (draw scene camera dt)
    (as-frame
     (draw (post scene) camera dt)
     ;;(draw-tex-tl (first (sam camera)) :color-scale (v! 10 10 10 1) )
     ;;(draw-tex-br (spot-sam *state*)))
     ;;(draw-tex-br (point-sam *state*)) :index 0
     )
    ;;(draw-tex-br (dir-sam *state*)) :index 0)
    )
  )

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))

(defun skitter-cleanup-controls (source slot-name)
  (loop :for listener :across (skitter-hidden::uvec2-control-listeners
                               (skitter::get-control source slot-name nil t))
        :do (skitter:stop-listening listener)))
(defun skitter-cleanup ()
  (skitter-cleanup-controls (skitter:window 0) :size))

(defun window-listener (dim)
  (setf (dim (current-camera))          (list (x dim) (y dim))
        (resolution (current-viewport)) (v! (x dim) (y dim))))
(defun window-listener-trampoline (&rest args)
  (window-listener (first args)))
