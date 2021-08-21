(in-package #:scenic)

(defun move (obj direction mult)
  (setf (pos obj) (v3:- (pos obj)
                        (v3:*s (q:rotate direction (rot obj)) mult))))

(defun rot-from (current-rot axis ang)
  (q:normalize
   (q:* current-rot (q:from-axis-angle axis ang))))
(defun turn (camera ang)
  (setf (rot camera) (rot-from (rot camera) *vec3-up*      ang)))
(defun tilt (camera ang)
  (setf (rot camera) (rot-from (rot camera) *vec3-right*   ang)))
(defun spin (camera ang)
  (setf (rot camera) (rot-from (rot camera) *vec3-forward* ang)))

(defun god-move (factor dt camera)
  "absolute movement"
  (declare (type fixnum factor) (type single-float dt))
  (let ((mult (* factor dt)))
    (when (key-down-p key.w)     (move camera *vec3-forward* mult))  ;; ↑ forward
    (when (key-down-p key.a)     (move camera *vec3-right*   mult))  ;; ← left
    (when (key-down-p key.s)     (move camera *vec3-back*    mult))  ;; ↓ backwards
    (when (key-down-p key.d)     (move camera *vec3-left*    mult))  ;; → right
    (when (key-down-p key.space) (move camera *vec3-down*    mult))
    (when (key-down-p key.c)     (move camera *vec3-up*      mult))))

(defun full-rot (factor dt camera)
  (let ((angle (radians .4)))
    (when (key-down-p key.q)     (spin camera    angle))
    (when (key-down-p key.e)     (spin camera (- angle)))
    (when (key-down-p key.left)  (turn camera    angle))
    (when (key-down-p key.right) (turn camera (- angle)))
    (when (key-down-p key.up)    (tilt camera    angle))
    (when (key-down-p key.down)  (tilt camera (- angle)))))

(defun mouse-spin (camera)
  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle *vec3-right* (- (y move)))
                        (q:from-axis-angle *vec3-up*    (- (x move)))))))))))
