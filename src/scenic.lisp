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
   (list (make-material :roughness .8 :metallic .02 :specular .1)
         (make-material :roughness .4 :metallic .4  :specular .1)))
  #+nil
  (let ((s1 (make-scene-ode :color (v! .2 .2 .2 1) :post (list (make-defer-postprocess)))))
    (register (make-defered :downscale .25 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))) s1)
    (register (make-box :w 20f0 :d 20f0 :pos (v! 0 -.5 0)) s1)
    (register (make-spot :far 20f0 :near 4f0 :pos (v! -2 8 -2) :color (v! .5 .1 .3) :rot (q:point-at (v! 0 1 0) (v! -2 8 -2) (v! 0 0 0))) s1)
    ;;(register (make-directional :pos (v! 150 333 223)) s1)
    (register s1 *state*))
  ;;#+nil
  (let ((s1 (make-scene :color (v! .2 .2 .2 1))))
    (register (make-perspective :downscale .25 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))) s1)
    (register (make-directional :pos (v! 150 333 223)) s1)
    (register s1 *state*))
  #+nio
  (let ((s1 (make-scene-ode :color (v! .2 .2 .2 1) :post (list (make-defer-postprocess)))))
    #+nil
    (register (make-defered :downscale .25 :pos (v! -14.65844 4.4909353 0.22071815)
                            :rot (q! 0.95456934 -0.0030270235 -0.297679 0.013245501))
              s1)
    (mapcar (lambda (a) (register a s1)) (make-building-physics (space s1)))
    (register (make-physic-camera :pos (v! -9.748983 6.841267 -0.19243619)
                                  :space (space s1)
                                  :downscale .25)
              s1)
    ;;(register (make-directional :pos (v! 150 333 223)) s1)
    (register (make-point :pos (v! -10 5  0) :far 20f0 :fudge 0.2)  s1)
    (register (make-point :pos (v! -11 8 -5) :far 20f0 :fudge 0.08) s1)
    (register s1 *state*)))


(defclass tick (event)
  ((tt :initarg :tt :reader tt)
   (dt :initarg :dt :reader dt)
   (fc :initarg :fc :reader fc)))

(def-simple-main-loop play-render (:on-start #'init)
  (main-loop))

(let ((fc 0)
      (tt 0.0d0)
      (dt 0.0d0)
      (current-time (current-time)))
  (declare (type double-float current-time tt dt))
  (defun init ()
    (setf fc 0 tt 0.0d0 current-time (current-time))
    (slynk-hook)
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
      (when (zerop (mod fc 5))
        (setf current-time (current-time))
        (setf dt (- tt current-time .001d0))
        (setf dt (if (> dt .16d0) .00001d0 dt))
        (setf tt current-time)
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

        ;;(draw-tex-br (first (sam (prev *state*))))
        ;;(draw-tex-bl (first (sam (next *state*))))

        ;;(draw-tex-tr (first (sam (dof-render-coc      (first (post scene))))))
        ;;(draw-tex-tr (first (sam (dof-render-bokeh    (first (post scene))))))
        ;;(draw-tex-br (first (sam (dof-render-coc-half (first (post scene))))))

        ;; (draw-tex-br (first (lights (current-scene))))

        ;; (draw-tex-tl (fourth (sam camera)));; ???
        ;; (draw-tex-tr (third  (sam camera))); NORMAL
        ;; (draw-tex-bl (second (sam camera))); POS
        ;; (draw-tex-br (first  (sam camera))); ALBEDO

        ;; (draw-tex-tr (first (sam (capture    scene))))
        ;; (draw-tex-tl (first (sam (prefilter  scene))))
        ;; (draw-tex-bl (first (sam (irradiance scene))))

        ;;(draw-tex-br (point-sam *state*) :index 0)
        ;;(draw-tex-br (spot-sam  *state*) :index 0)
        ;;(draw-tex-br (dir-sam   *state*) :index 0)
        ))))

(defun start ()
  (play-render :start))

(defun stop ()
  (play-render :stop))
