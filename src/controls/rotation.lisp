(in-package #:scenic)

(defun full-rot (factor dt camera)
  (let ((angle (coerce (* factor (abs dt)) 'single-float)))
    (when (key-down-p key.q)    (spin camera    angle))
    (when (key-down-p key.e)    (spin camera (- angle)))
    ;; NOTE: without being careful "tilt" spins the camera !?
    (when (key-down-p key.up)   (tilt camera    angle))
    (when (key-down-p key.down) (tilt camera (- angle)))
    (human-rot factor dt camera)))

(defun tank-rot (factor dt camera)
  (let ((angle (coerce (* factor (abs dt)) 'single-float)))
    (when (or (key-down-p key.a) (key-down-p key.left))
      (turn camera    angle))
    (when (or (key-down-p key.d) (key-down-p key.right))
      (turn camera (- angle)))))

(defun human-rot (factor dt camera)
  (let ((angle (coerce (* factor (abs dt)) 'single-float)))
    (when (key-down-p key.left)  (turn camera    angle))
    (when (key-down-p key.right) (turn camera (- angle)))))

(defun rot-from (current-rot axis ang)
  (q:normalize
   (q:* current-rot (q:from-axis-angle axis ang))))
(defun turn (camera ang)
  (setf (rot camera) (rot-from (rot camera) *vec3-up*      ang)))
(defun tilt (camera ang)
  (setf (rot camera) (rot-from (rot camera) *vec3-right*   ang)))
(defun spin (camera ang)
  (setf (rot camera) (rot-from (rot camera) *vec3-forward* ang)))
