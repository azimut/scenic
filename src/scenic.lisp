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
  (init-state
   `(,(make-material :roughness .8 :metallic .02 :specular .1)
     ,(make-material :roughness .4 :metallic .4  :specular .1))))

(def-simple-main-loop play-render (:on-start #'init)
  (main-loop))

(let ((fc 0)
      (tt 0.0d0)
      (dt 0.0d0)
      (current-time (current-time)))
  (declare (type double-float current-time tt dt))
  (defun init ()
    (slynk-hook)
    (setf fc 0 tt 0.0d0 current-time (current-time))
    (log4cl:log-info (uiop:getcwd)
                     (uiop:getenv "APPIMAGE")
                     (uiop:getenv "APPDIR"))
    (free *state*)
    (ode-stop-world)
    (reset-material-counter)
    (skitter-cleanup)
    (skitter-listen)
    (ode-init-world)
    (init-all-the-things)
    (setf (last-time *state*) (get-internal-real-time)))
  (defun main-loop ()
    (when (or (key-down-p key.escape))
      (stop))
    (let* ((scene  (current-scene))
           (camera (active-camera scene)))
      (when (zerop (mod fc 1));; TODO: proper 60 FPS tick
        (setf current-time (current-time))
        (setf dt (- tt current-time .001d0))
        (setf dt (if (> dt .16d0) .00001d0 dt))
        (setf tt current-time)
        ;;#+nil
        (when (< (abs dt) #.(/ 1f0 60f0))
          (sleep (- #.(/ 1f0 60f0) (abs dt))))
        (issue scene 'tick :tt tt :dt dt :fc fc))
      (upload scene)
      (update scene dt)
      ;; (rocket-update)
      (decay-events)
      ;; Lights/Shadows
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
      (incf fc)
      (as-frame
        (blit scene (post scene) camera dt)

        ;; (draw-tex-bl (first (sam (prev *state*))))
        ;; (draw-tex-br (first (sam (next *state*))))

        ;;(draw-tex-tr (first (sam (dof-render-coc      (first (post scene))))))
        ;;(draw-tex-tr (first (sam (dof-render-bokeh    (first (post scene))))))
        ;;(draw-tex-br (first (sam (dof-render-coc-half (first (post scene))))))

        ;; (draw-tex-br (first (lights (current-scene))))

        ;; (draw-tex-tl (first  (sam camera))); ALBEDO
        ;; (draw-tex-tr (second (sam camera))); POS
        ;; (draw-tex-bl (third  (sam camera))); NORMAL
        ;; (draw-tex-br (fourth (sam camera))) ;; ???
        ;; (draw-tex    (fifth (sam camera)) :scale .5) ;; DEPTH

        ;; (draw-tex-tl (first (sam (capture    scene))))
        ;; (draw-tex-tr (first (sam (irradiance scene))))
        ;; (draw-tex-bl (first (sam (prefilter  scene))))
        ;; (draw-tex-br (brdf-sam *state*))

        ;; (draw-tex-br (point-sam *state*) :index 0)
        ;; (draw-tex-br (spot-sam  *state*) :index 0)
        ;; (draw-tex-br (dir-sam   *state*) :index 0)
        ))))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))
