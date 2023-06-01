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

