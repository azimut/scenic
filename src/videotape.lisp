(in-package #:scenic)

(defvar *tape* nil
  "contains an instance of VIDEOTAPE, used on RECORD-LOOP")

;; TODO: .rocket filepath
(defclass videotape (renderable)
  ((process  :reader  videotape-process)
   (duration :initarg :duration :documentation "duration in seconds" :reader duration)
   (fps      :initarg :fps      :documentation "frames per second" :reader fps)
   (filename :initarg :filename :documentation "output video filename"))
  (:default-initargs
   :duration 10
   :filename "output.mp4"
   :fps 30
   :texture-opts '((0 :element-type :rgb8)) ;; LDR - CEPL names rgb24=rgb8?
   :sample-opts '((:wrap :clamp-to-edge)))
  (:documentation "records *STATE* into video FILENAME"))

(defmethod initialize-instance :before ((obj videotape) &key filename)
  (flet ((valid-filename-p (f)
           (string-equal "mp4" (subseq (string-downcase f) (- (length f) 3)))))
    (assert (valid-filename-p filename))))

(defmethod initialize-instance :after ((obj videotape) &key dim)
  (destructuring-bind (width height) dim
    (with-slots (process filename fps) obj
      (setf process
            (uiop:launch-program
             (format nil "ffmpeg -y -f rawvideo -pix_fmt rgb24 -s ~dx~d -r ~d -an -i - -c:v libx264 ~a"
                     width
                     height
                     fps
                     filename)
             :input :stream
             :output nil
             :error-output nil)))))

(defmethod free ((obj videotape))
  (uiop:close-streams     (videotape-process obj))
  (uiop:terminate-process (videotape-process obj))
  (uiop:wait-process      (videotape-process obj)))

(defun record-loop ()
  (unless *rocket* (return-from record-loop))
  ;; scene -> tape
  (map-g-into (fbo *tape*)
              #'fxaa3-pipe (bs *state*)
              :one-over-res (v2:/ (v! 1 1) (viewport-resolution (current-viewport)))
              :settings (v! 0.75 0.166 0.0833)
              :sam (first (sam (prev *state*))))
  ;;  tape -> ffmpeg
  (let ((c-array (pull1-g (first (tex *tape*)))))
    (destructuring-bind (width height) (dim *tape*)
      (loop :for i :from (1- (* width height)) :downto 0 :do
        (write-sequence
         (row-major-aref-c c-array i)
         (uiop:process-info-input (videotape-process *tape*))))))
  ;; TICK
  (let ((step-size (/ (rocketman::state-rps *rocket*) (fps *tape*))));; TODO: rps?
    (incf (rocketman::state-row *rocket*)
          step-size)))

(defun record-init (&aux (width  (first  (dim *tape*))) (height (second (dim *tape*))))
  (init)
  ;; Once we have an event-loop, ask for prev/next to resize
  (issue (current-scene) 'resize :width width :height height)
  (main-loop)
  (main-loop)); with an extra draw we make sure everything is ready (eg: ibl)

(def-simple-main-loop record-render (:on-start #'record-init)
  (main-loop)
  (record-loop))

(defmacro with-videotape ((fps duration filename width height) &body body)
  `(progn
     (setf *tape* (make-instance
                   'videotape
                   :fps ,fps
                   :duration ,duration
                   :filename ,filename
                   :dim (list ,width ,height)))
     (unwind-protect (progn ,@body)
       (free *tape*))))

(defun record (&key (fps 30) (duration 1) (filename "output.mp4") (width 800) (height 600))
  (with-videotape (fps duration filename width height)
    (with-viewport (make-viewport `(,width ,height))
      (record-render :stop)
      (record-render :start (* fps duration))))
  (skitter-cleanup))
