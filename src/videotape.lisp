(in-package #:scenic)

(defvar *tape* nil
  "contains an instance of VIDEOTAPE, used on RECORD-LOOP")

;; TODO: .rocket filepath
(defclass videotape (renderable)
  ((process  :reader  process)
   (seconds  :initarg :seconds  :documentation "duration in seconds" :reader seconds)
   (fps      :initarg :fps      :documentation "frames per second" :reader fps)
   (filename :initarg :filename :documentation "output video filename"))
  (:default-initargs
   :seconds 10
   :filename "output.mp4"
   :fps 30
   :dim (viewport-dimensions (current-viewport))
   :texture-opts '((0 :element-type :rgb8)) ;; CEPL names rgb24=rgb8?
   :sample-opts '((:wrap :clamp-to-edge)))
  (:documentation "records *STATE* into video FILENAME"))

(defmethod initialize-instance :after ((obj videotape) &key)
  (destructuring-bind (width height)
      (viewport-dimensions (current-viewport))
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
  (uiop:close-streams     (process obj))
  (uiop:terminate-process (process obj))
  (uiop:wait-process      (process obj)))

(defun record-loop ()
  (unless *rocket* (return-from record-loop))
  ;; scene -> tape
  (map-g-into (fbo *tape*)
              #'hdr-acesfilm-pipe (bs *state*)
              :sam (first (sam (prev *state*)))
              :exposure .9)
  ;;  tape -> ffmpeg
  (let ((c-array (pull1-g (first (tex *tape*)))))
    (destructuring-bind (width height)
        (viewport-dimensions (current-viewport))
      (dotimes (i (* width height))
        (write-sequence
         (row-major-aref-c c-array i)
         (uiop:process-info-input (process *tape*))))))
  ;; TICK
  (let ((step-size (/ (rps *rocket*) (fps *tape*))))
    (incf (rocketman::state-row *rocket*)
          step-size)))

(def-simple-main-loop record-render (:on-start #'init)
  (record-loop)
  (main-loop))

(defun record (&key (fps 30) (seconds 1))
  (free *tape*)
  (setf *tape* (make-instance 'videotape :fps fps :seconds seconds))
  (record-render :stop)
  (record-render :start (* fps seconds))
  (free *tape*))
