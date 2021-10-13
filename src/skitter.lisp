(in-package #:scenic)

(defun skitter-cleanup ()
  (skitter-cleanup-controls (skitter:window 0) :size))

(defun skitter-cleanup-controls (source slot-name)
  (loop :for listener :across (skitter-hidden::uvec2-control-listeners
                               (skitter::get-control source slot-name nil t))
        :do (skitter:stop-listening listener)))

(defun window-listener (dim)
  (let ((width (x dim)))
    (setf (resolution (current-viewport)) (v! width (* 0.5625 width)))
    (issue (current-scene) 'resize :width width :height (* width 0.5625))))

(defun window-listener-trampoline (&rest args)
  (window-listener (first args)))
