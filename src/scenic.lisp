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
  (reset-pbr-counter)
  (skitter-cleanup)
  (skitter:listen-to #'window-listener-trampoline (skitter:window 0) :size)
  (init-state
   (list (make-pbr))
   (list
    (make-scene
     (list (make-perspective
            :pos (v! 2 2 2)
            :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
     (make-lights
      :dir-lights nil
      ;; (list (let* ((pos (v! -50 30 20))
      ;;              (dis (v3:distance pos (v! 0 0 0))))
      ;;         (make-directional
      ;;          :pos pos
      ;;          :rot (q:point-at (v! 0 1 0) pos (v! 0 0 0))
      ;;          :far (+ dis (* dis .1))
      ;;          :near (- dis (* dis .1))
      ;;          :fs (v2! 10))))
      :point-lights
      (list
       (make-point
        :pos (v! -2 2 -2)
        :color (v! .1 .1 .3)
        :linear 0.35 :quadratic 0.44)
       (make-point
        :pos (v! 2 2 2)
        :color (v! .8 .2 .6)
        :linear 0.35 :quadratic 0.44)
       ))
     (make-simple-postprocess))))
  ;; Actors
  (push (make-actor :w 10f0 :d 10f0) (actors (current-scene)))
  ;;#+nil
  (dotimes (i 5)
    (let ((a (make-actor)))
      (setf (pos a) (v! (- (random 5) 2.5) 1 (- (random 5) 2.5)))
      (setf (rot a) (q:from-axis-angle (v! 0 1 0) (radians (random 180))))
      (push a (actors (current-scene)))))
  (push (make-actor :h 5f0) (actors (current-scene)))

  )

(let ((time 0f0))
  (def-simple-main-loop play-render (:on-start #'init)
    (let* ((scene  (current-scene))
           (camera (active-camera scene))
           ;;(time 0f0)
           (dt 0f0))
      (dolist (m (materials *state*))
        (upload m))
      (upload (lights scene))
      (update scene dt)
      (draw scene (lights scene) time)
      (draw scene camera time)
      (as-frame
        (draw (post scene) camera time)
        ;;(draw-tex-tl (first (sam camera)) :color-scale (v! 10 10 10 1) )
        ;;(draw-tex-br (point-sam (lights scene)) :index 1)
        ;;(draw-tex-br (dir-sam (lights scene)) :index 0)
        )
      ;;(incf time .001)
      )))

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
