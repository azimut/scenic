(in-package #:scenic)

;;------------------------------
;; Movement

(defun move (obj direction mult)
  (setf (pos obj)
        (v3:+ (pos obj)
              (v3:*s (q:rotate direction (rot obj)) mult))))


(defun human-move (factor dt camera)
  (declare (type single-float factor) (type double-float dt) )
  (let ((mult (coerce (* factor (abs dt)) 'single-float)))
    ;; ↑ forward
    (when (keyboard-button (keyboard) key.w)
      (let* ((camdir (q:rotate *vec3-back* (rot camera)))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        (setf (pos camera) (v3:+ (pos camera) dt-pos))))
    ;; ↓ backwards
    (when (keyboard-button (keyboard) key.s)
      (let* ((camdir (q:to-direction (rot camera)))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        (setf (pos camera) (v3:- (pos camera) dt-pos))))
    ;; ← left
    (when (keyboard-button (keyboard) key.a)
      (let* ((camdir (q:rotate *vec3-left* (rot camera)))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        (setf (pos camera) (v3:+ (pos camera) dt-pos))))
    ;; → right
    (when (keyboard-button (keyboard) key.d)
      (let* ((camdir (q:rotate *vec3-right* (rot camera)))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        (setf (pos camera) (v3:+ (pos camera) dt-pos))))
    ;; - up - jump
    (when (keyboard-button (keyboard) key.space)
      (setf (pos camera) (v3:+ (pos camera)
                               (v3:*s (q:rotate *vec3-up* (rot camera))
                                      mult))))
    ;; - down - croutch
    (when (keyboard-button (keyboard) key.c)
      (setf (pos camera) (v3:+ (pos camera)
                               (v3:*s (q:rotate *vec3-down* (rot camera))
                                      mult))))))

(defun ode-move (camera dir3)
  (when (zerop (%ode:body-is-enabled (body camera)))
    (%ode:body-enable (body camera)))
  (let ((dir3 (v3:*s dir3 10f0)))
    (%ode:body-set-linear-vel (body camera)
                              (x dir3)
                              0.8
                              (z dir3))))

(defun human-move-ode (factor dt camera)
  (when (plusp (%ode:body-is-enabled (body camera)))
    (setf (pos camera) (ode-geom-get-position (geom camera))))
  (when (key-down-p key.w) ;; ↑
    (ode-move camera (q:to-direction (rot camera))))
  (when (key-down-p key.s) ;; ↓
    (ode-move camera (v3:* (q:to-direction (rot camera))
                           (v! -1 0 -1))))
  (when (key-down-p key.a) ;; ←
    (ode-move camera (q:rotate *vec3-left* (rot camera))))
  (when (key-down-p key.d) ;; →
    (ode-move camera (q:rotate *vec3-right* (rot camera))))
  (when (key-down-p key.space) ;; - up - jump
    (when (zerop (%ode:body-is-enabled (body camera)))
      (%ode:body-enable (body camera)))
    (%ode:body-set-linear-vel (body camera) 0f0 5f0 0f0)))

#+nil
(defun human-move (factor dt camera)
  (declare (type single-float factor) (type double-float dt) )
  (let ((mult (coerce (* factor dt) 'single-float)))
    ;; ↑ forward
    (when (key-down-p key.w)
      (let* ((camdir (v3:*s (q:to-direction (rot camera)) 10f0))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        ;;(setf (pos camera) (v3:- (pos camera) dt-pos))
        (%ode:body-set-linear-vel (body camera) (x camdir) 0f0 (z camdir))
        ))
    ;; ↓ backwards
    (when (key-down-p key.s)
      (let* ((camdir (v3:*s (q:to-direction (rot camera)) 10f0))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        ;;(setf (pos camera) (v3:+ (pos camera) dt-pos))
        (%ode:body-set-linear-vel (body camera)
                                  (- (x camdir))
                                  0f0
                                  (- (z camdir)))
        ))
    ;; ← left
    (when (key-down-p key.a)
      (let* ((camdir (q:rotate *vec3-right* (rot camera)))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        (setf (pos camera) (v3:+ (pos camera) dt-pos))))
    ;; → right
    (when (key-down-p key.d)
      (let* ((camdir (q:rotate *vec3-left* (rot camera)))
             (dt-pos (v3:*s camdir mult))
             (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
        (setf (pos camera) (v3:+ (pos camera) dt-pos))))
    ;; - up - jump
    (when (key-down-p key.space)
      (%ode:body-set-linear-vel (body camera) 0f0 5f0 0f0)
      #+nil
      (setf (pos camera) (v3:- (pos camera)
                               (v3:*s (q:rotate *vec3-up* (rot camera))
                                      mult))))
    ;; - down - croutch
    (when (key-down-p key.c)
      (setf (pos camera) (v3:- (pos camera)
                               (v3:*s (q:rotate *vec3-down* (rot camera))
                                      mult))))))

(defun god-move (factor dt camera)
  (declare (type single-float factor) (type double-float dt))
  (let ((mult (coerce (* factor (abs dt)) 'single-float)))
    (when (key-down-p key.w)     (move camera *vec3-forward* mult)) ;; ↑
    (when (key-down-p key.a)     (move camera *vec3-left*    mult)) ;; ←
    (when (key-down-p key.s)     (move camera *vec3-back*    mult)) ;; ↓
    (when (key-down-p key.d)     (move camera *vec3-right*   mult)) ;; →
    (when (key-down-p key.space) (move camera *vec3-up*      mult))
    (when (key-down-p key.c)     (move camera *vec3-down*    mult)))
  (pos camera))

;;------------------------------
;; Rotation

(defun full-rot (factor dt camera)
  (let ((angle (coerce (* factor (abs dt)) 'single-float)))
    (when (key-down-p key.q)    (spin camera    angle))
    (when (key-down-p key.e)    (spin camera (- angle)))
    ;; NOTE: without being careful "tilt" spins the camera !?
    (when (key-down-p key.up)   (tilt camera    angle))
    (when (key-down-p key.down) (tilt camera (- angle)))
    (human-rot factor dt camera)))

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

;;------------------------------
;; Mouse

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
