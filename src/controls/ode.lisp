(in-package #:scenic)

;; References:
;; - https://github.com/iomeone/raylib-ode-fps/blob/main/app/example_app.c
;; - http://ode.org/wiki/index.php?title=HOWTO_upright_capsule
;; - http://ode.org/wiki/index.php?title=Manual#How_can_I_make_my_actor_capsule_.28capped_cylinder.29_stay_upright.3F
;;

(defun crouch (camera &aux (body (body camera)))
  (when (zerop (%ode:body-is-enabled body))
    (%ode:body-enable body))
  (%ode:body-set-linear-vel body 0f0 -20f0 0f0))

(defun jump (camera &aux (body (body camera)))
  (when (zerop (%ode:body-is-enabled body))
    (%ode:body-enable body))
  (cffi-c-ref:c-let ((vel %ode:real :from (%ode:body-get-linear-vel body)))
    (%ode:body-set-linear-vel body (* (vel 0) .9) 6f0 (* (vel 2) .9))))

;;--------------------------------------------------
;; Tank Controls - Two directions
(defun ode-move (camera dir3 factor &aux (body (body camera)))
  (when (zerop (%ode:body-is-enabled body))
    (%ode:body-enable body))
  (cffi-c-ref:c-let ((vel %ode:real :from (%ode:body-get-linear-vel (body camera))))
    (let ((new-dir3 (v3:*s (v3:+ dir3 (v! (vel 0) (vel 1) (vel 2))) factor)))
      (%ode:body-set-linear-vel body (x new-dir3) (y new-dir3) (z new-dir3)))))

(defmethod tank-move-ode (camera factor)
  ;; Visually move it
  (when (plusp (%ode:body-is-enabled (body camera)))
    (setf (pos camera) (ode-geom-get-position (geom camera))))

  (when (hit-floor-p camera)
    (setf (running-p camera) (key-down-p key.lshift))
    (cond ((or (key-down-p key.w) (key-down-p key.up)) ;; ↑
           (ode-move camera (q:to-direction (rot camera))
                     (if (running-p camera) (* factor 1.1) factor)))
          ((or (key-down-p key.s) (key-down-p key.down)) ;; ↓
           (ode-move camera (v3:*s (q:to-direction (rot camera)) -1f0)
                     (if (running-p camera) (* factor 1.1) factor))))
    (when (key-down-p key.space)
      (jump camera))
    (when (key-down-p key.c)
      (crouch camera))))

;;--------------------------------------------------
;; FPS Controls - Four directions
;; TODO: movement combinations, duplicate forces

(defmethod human-move-ode
    ((camera physic-camera) factor)
  ;; Visually move it
  (when (plusp (%ode:body-is-enabled (body camera)))
    (setf (pos camera) (ode-geom-get-position (geom camera))))

  (when (hit-floor-p camera)
    (setf (running-p camera) (key-down-p key.lshift))
    (when (key-down-p key.w) ;; ↑
      (ode-move camera (q:to-direction (rot camera))
                (if (running-p camera) (* factor 1.1) factor)))
    (when (key-down-p key.s) ;; ↓
      (ode-move camera (v3:*s (q:to-direction (rot camera)) -1f0)
                (if (running-p camera) (* factor 1.1) factor)))
    (when (key-down-p key.a) ;; ←
      (ode-move camera (q:rotate *vec3-left* (rot camera))
                (if (running-p camera) (* factor 1.1) factor)))
    (when (key-down-p key.d) ;; →
      (ode-move camera (q:rotate *vec3-right* (rot camera))
                (if (running-p camera) (* factor 1.1) factor)))
    (when (key-down-p key.c)
      (crouch camera))
    (when (key-down-p key.space)
      (jump camera))))
