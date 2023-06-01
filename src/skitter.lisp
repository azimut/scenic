(in-package #:scenic)

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

(defun window-listener (dim)
  (let* ((w (x dim)) (h (* w 0.5625)))
    (setf (resolution (current-viewport)) (v! w h))
    (issue (current-scene) 'resize :height h :width w)))
