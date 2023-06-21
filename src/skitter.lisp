(in-package #:scenic)

(defvar *downscale* 1f0
  "Dowscales the value sent by the RESIZE event")

(defun skitter-listen ()
  (skitter:listen-to #'window-size-trampoline
                     (skitter:window 0)
                     :size)
  (skitter:listen-to #'window-stop-trampoline
                     skitter:+window-manager+
                     :quitting))

(defun skitter-cleanup ()
  (skitter-cleanup-controls (skitter:window 0) :size))

(defun skitter-cleanup-controls (source slot-name)
  (loop :for listener
          :across
          (skitter-hidden::uvec2-control-listeners
           (skitter::get-control source slot-name nil t))
        :do (skitter:stop-listening listener)))

(defun window-stop-trampoline (&rest args)
  (declare (ignore args))
  (stop))

(defun window-size-trampoline (&rest args)
  (window-listener (first args)))

(defun window-listener (dim &aux (width (x dim)) (height (y dim)))
  (let* ((w (if (> height width) width  (* height (/ 16 9f0))))
         (h (if (> width height) height (* width  (/ 9 16f0)))))
    (setf (resolution (current-viewport)) (v! w h))
    (when (scenes *state*)
      (issue (current-scene)
             'resize
             :height (round (* *downscale* h))
             :width  (round (* *downscale* w))))))
