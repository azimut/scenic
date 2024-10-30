(in-package #:scenic)

(defvar *rocket* nil)

(defun rocket-free ()
  (when *rocket*
    (rocketman:disconnect *rocket*)
    (setf *rocket* nil)))

(defun rocket-init (&optional standalone-p)
  (unless *rocket*
    (setf *rocket* (rocketman:make-rocket :rps 8))
    (unless standalone-p
      (rocketman:connect *rocket*))))

(defun rocket-reset (&optional standalone-p)
  (rocket-free)
  (rocket-init standalone-p)
  (sleep 0.5))

(defmacro with-rocket-commands (&body body)
  "flush messages after sending commands"
  `(when *rocket*
     ,@body
     (when-let* ((socket (rocketman::con-socket *rocket*))
                 (stream (usocket:socket-stream socket)))
         (loop :while (listen stream)
               :do (ecase (read-byte stream)
                     (0 (rocketman::handle-set-key     *rocket* stream))
                     (1 (rocketman::handle-delete-key  *rocket* stream))
                     (3 (rocketman::handle-set-row     *rocket* stream))
                     (4 (rocketman::handle-pause       *rocket* stream))
                     (5 (rocketman::handle-save-tracks *rocket* stream)))
                   (sleep .02)))
     (rocket-set-row 0)))

(defun rocket-update ()
  (when *rocket*
    (rocketman:update *rocket*)))

(defun rocket-add (track)
  (declare (type string track))
  (rocketman:add-track *rocket* track))

(defun rocket-get (track)
  (declare (type string track))
  (if *rocket*
      (coerce (rocketman:get-track *rocket* track)
              'single-float)
      0f0))

(defun rocket-get-v3 (x y z)
  (if *rocket*
      (v! (rocketman:get-track *rocket* x)
          (rocketman:get-track *rocket* y)
          (rocketman:get-track *rocket* z))
      (v! 0 0 0)))

(defun rocket-set-row (value)
  (rocketman:set-row *rocket* value))

(defun rocket-set-rps (value)
  (check-type value unsigned-byte)
  (setf (slot-value *rocket* 'rocketman::rps) value))

(defun rocket-pause-toggle ()
  (when *rocket*
    (rocketman::toggle-pause *rocket*)))

(defun rocket-pause ()
  (when *rocket*
    (rocketman::pause-it *rocket* 1)))

(defun rocket-play ()
  (when *rocket*
    (rocketman::pause-it *rocket* 0)))

(defun rocket-row ()
  (slot-value *rocket* 'rocketman::row))

(defmethod (setf rocket-row) (new-value)
  (rocketman:set-row *rocket* (max 0 new-value)))

(defun rocket-load-file (file)
  (rocketman:load-file *rocket* (truename file)))

;;--------------------------------------------------

#+nil
(let ((stepper (make-stepper (seconds .2) (seconds .2))))
  (defmethod draw! :around ()
    (print "updating")
    (rocket-update)
    #+nil
    (when (and (key-down-p key.space) (funcall stepper))
      (rocket-pause-toggle))
    (call-next-method)))
