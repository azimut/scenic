(in-package #:scenic)

(defvar *tape* nil
  "contains an instance of VIDEOTAPE, used on RECORD-LOOP")

;; TODO: .rocket filepath
(defclass videotape (renderable)
  ((process  :reader  process)
   (duration :initarg :duration :documentation "duration in seconds" :reader duration)
   (fps      :initarg :fps      :documentation "frames per second" :reader fps)
   (filename :initarg :filename :documentation "output video filename"))
  (:default-initargs
   :duration 10
   :filename "output.mp4"
   :fps 30
   :dim (viewport-dimensions (current-viewport))
   :texture-opts '((0 :element-type :rgb8)) ;; CEPL names rgb24=rgb8?
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
    (destructuring-bind (width height) (dim *tape*)
      (loop :for i :from (1- (* width height)) :downto 0 :do
        (write-sequence
         (row-major-aref-c c-array i)
         (uiop:process-info-input (process *tape*))))))
  ;; TICK
  (let ((step-size (/ (rocketman::state-rps *rocket*) (fps *tape*))));; TODO: rps?
    (incf (rocketman::state-row *rocket*)
          step-size)))

(defun record-init ()
  (setf (viewport-dimensions (current-viewport)) (dim *tape*))
  (init)
  (main-loop)); with an extra draw we make sure everything is ready (eg: ibl)

(def-simple-main-loop record-render (:on-start #'record-init)
  (main-loop)
  (record-loop))

(defmacro with-videotape ((fps duration filename width height) &body body)
  `(progn
     (setf *tape* (make-instance
                   'videotape
                   :fps ,fps :duration ,duration :filename ,filename :dim (list ,width ,height)))
     ,@body
     (free *tape*)))

(defun record (&key (fps 30) (duration 1) (filename "output.mp4") (width 800) (height 600))
  (with-videotape (fps duration filename width height)
    (record-render :stop)
    (record-render :start (* fps duration))))
