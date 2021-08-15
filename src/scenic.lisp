(in-package #:scenic)


(defvar *shadow-fbo* nil)
(defvar *shadow-sam* nil)
(defun init-shadow ()
  (unless *shadow-fbo*
    (setf *shadow-fbo* (make-fbo `(:d :dimensions (1024 1024)
                                      ;;:element-type :depth-component32
                                      ))))
  (unless *shadow-sam*
    (setf *shadow-sam* (sample (attachment-tex *shadow-fbo* :d)
                               :wrap           :clamp-to-border
                               :minify-filter  :nearest
                               :magnify-filter :nearest))
    (setf (cepl.samplers::border-color *shadow-sam*) (v! 1 1 1 1))))


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
  (skitter-cleanup)
  (init-shadow)
  (skitter:listen-to #'window-listener-trampoline (skitter:window 0) :size)
  (init-state
   (list
    (make-scene
     (list (make-perspective
            :pos (v! 2 2 2)
            :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
     (make-lights
      :dir-lights
      (list (make-directional
             :pos (v! 100 50 50)
             :rot (q:point-at (v! 0 1 0) (v! 100 100 100) (v! 0 0 0))))
      :point-lights
      (list (make-point :pos (v! 8 10 10) :color (v! 0 1 0)
                        :linear 0.14
                        :quadratic 0.07)))
     (make-simple-postprocess))))
  (push (make-actor :h 5f0) (actors (current-scene)))
  (push (make-actor :w 10f0 :d 10f0) (actors (current-scene)))
  )

(def-simple-main-loop play-render (:on-start #'init)
  (let* ((scene  (current-scene))
         (actors (actors scene))
         (camera (active-camera scene))
         (time 0f0)
         (dt 0f0))
    (draw scene (lights scene) time)
    (draw scene camera time)
    (update scene dt)
    (as-frame
      (draw (post scene) camera time)
      ;;(draw-tex-br *shadow-sam*)
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
  (setf (dim (current-camera)) (list (x dim) (y dim)))
  (setf (resolution (current-viewport)) (v! (x dim) (y dim))))
(defun window-listener-trampoline (&rest args)
  (window-listener (first args)))
