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

(defun init-all-the-things ()
  (init-state (list (make-material :roughness .8 :metallic .02 :specular .1)
                    (make-material :roughness .4 :metallic .4  :specular .1)))
  (let ((s1 (make-scene)))
    (register (make-perspective :scale .25 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))) s1)
    (register (make-point :pos (v! -2 2 -2) :color (v! .1 .1 .3) :linear 0.35 :quadratic 0.44) s1)
    (register (make-box) s1)
    (register s1 *state*)))

(defun init ()
  (free *state*)
  (ode-destroy)
  (slynk-hook)
  (reset-material-counter)
  (skitter-cleanup)
  (skitter:listen-to #'window-listener-trampoline (skitter:window 0) :size)
  (ode-init)
  (init-all-the-things)
  (setf (last-time *state*) (get-internal-real-time)))

(def-simple-main-loop play-render (:on-start #'init)
  (let* ((scene  (current-scene))
         (camera (active-camera scene))
         (now    (get-internal-real-time))
         (dt     (* (- now (last-time *state*) .001)))
         (dt     (if (> dt .16) .00001 dt)))

    (setf (last-time *state*) now)

    (ode-update)
    (upload scene)
    (update scene dt)

    (rocket-update)

    (decay-events)

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
    (process scene)
    (as-frame
      (blit scene (post scene) camera dt)
      ;; (draw-tex-br (first (sam camera)))
      ;; (draw-tex-bl (second (sam camera)))
      ;; (draw-tex-tr (third (sam camera)))
      ;; (draw-tex-tl (fourth (sam camera)))
      ;; (draw-tex-tr (first (sam (capture    scene))))
      ;; (draw-tex-tl (first (sam (prefilter  scene))))
      ;; (draw-tex-bl (first (sam (irradiance scene))))
      ;; (draw-tex-br (first (sam *irradiance*)))
      ;;(draw-tex-tl (first (sam camera)) :color-scale (v! 10 10 10 1) )
      ;;(draw-tex-br (spot-sam *state*)))
      ;;(draw-tex-br (point-sam *state*)  :index 0)
      ;;(draw-tex-br (dir-sam *state*)) :index 0)
      )
    )
  )

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))

