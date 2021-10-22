(in-package #:scenic)

;; NO idea how jointgroup is gonna be

(defvar *world* nil)
(defvar *space* nil)
(defvar *contactgroup* nil)

(defmethod collide (o1 o2))

(defun ode-init ()
  (unless *world*
    (%ode:init-ode)
    (setf *world*        (%ode:world-create)
          *space*        (%ode:hash-space-create nil)
          *contactgroup* (%ode:joint-group-create 0))
    (%ode:create-plane *space* 0f0 1f0 0f0 0f0))
  (%ode:world-set-gravity *world* 0f0 -9.8f0 0f0)
  ;; CFM = Constraint Force Mixing
  ;; changes stiffness of a joint.
  (%ode:world-set-cfm                       *world* 1f-5)
  ;; ERP = Error Reduction Parameter = [0,1]
  ;; corrects  joint error
  (%ode:world-set-erp                       *world* .1f0)
  (%ode:world-set-auto-disable-flag         *world* 1)
  (%ode:world-set-quick-step-w              *world* 1.3f0)
  (%ode:world-set-quick-step-num-iterations *world* 40)
  t)

(defun ode-destroy ()
  (when (and *space* *world* *contactgroup*)
    (%ode:joint-group-destroy *contactgroup*)
    (%ode:space-destroy *space*)
    (%ode:world-destroy *world*)
    (%ode:close-ode)
    (setf *world* nil
          *space* nil
          *contactgroup* nil)))

(progn
  (block gg
    (cffi:defcallback near-callback :void ((data :pointer)
                                           (o1 %ode:geom-id)
                                           (o2 %ode:geom-id))
      (declare (ignore data))
      (let ((b1 (%ode:geom-get-body o1))
            (b2 (%ode:geom-get-body o2)))
        (when (and (not (cffi:null-pointer-p b1))
                   (not (cffi:null-pointer-p b2))
                   (plusp (%ode:are-connected-excluding b1 b2 4 ;;%ode:+joint-type-contact+
                                                        )))
          (return-from gg))
        (cffi-c-ref:c-with ((contact %ode:contact :alloc t :count 5))
          (dotimes (i 5)
            (setf (contact i :surface :mode) (logior %ode:+contact-bounce+
                                                     %ode:+contact-slip1+
                                                     ;; %ode:+contact-slip2+
                                                     ;; %ode:+contact-approx1+
                                                     ;; %ode:+contact-soft-erp+
                                                     %ode:+contact-soft-cfm+)
                  (contact i :surface :slip1) .7f0
                  ;; (contact i :surface :slip2) .7f0
                  ;;(contact i :surface :soft-erp) .96f0
                  ;; friction parameter
                  (contact i :surface :mu) ode:+infinity+
                  (contact i :surface :mu2) 0f0
                  ;; bounce is the amount of "bouncyness"
                  (contact i :surface :bounce) .1f0
                  ;; bounce_vel is the minimum incoming velocity to cause a bounce
                  (contact i :surface :bounce-vel) .1f0
                  ;; constraint force mixing parameter
                  (contact i :surface :soft-cfm) .01f0))
          (let ((numc (%ode:collide o1 o2 10
                                    (contact :geom &)
                                    (cffi:foreign-type-size '%ode:contact))))
            (when (plusp numc)
              (when (and b1 b2))
              (dotimes (i numc)
                (%ode:joint-attach (%ode:joint-create-contact *world*
                                                              *contactgroup*
                                                              (contact i))
                                   b1 b2))))))))
  (let ((stepper (make-stepper (seconds .01) (seconds .01))))
    (defun ode-update ()
      "updates the objets within the physics engine"
      (when (and *world* (funcall stepper))
        (%ode:space-collide *space* nil (cffi:callback near-callback))
        (%ode:world-quick-step *world* 0.0099f0)
        (%ode:joint-group-empty *contactgroup*)))))

;; FIME: leaking? c-with would free it...
(defun ode-geom-get-position (geom)
  (declare (type sb-sys:system-area-pointer geom))
  (cffi-c-ref:c-let ((ode-pos %ode:real :from (%ode:geom-get-position geom)))
    (v! (ode-pos 0) (ode-pos 1) (ode-pos 2))))

(defun ode-geom-get-quaternion2 (orot geom)
  "returns a new rtg-math quaternion from the current rotation in the ODE geometry. Using the already C allocated OROT quaternion passed"
  (declare (type sb-sys:system-area-pointer orot geom))
  (%ode:geom-get-quaternion geom orot)
  (cffi-c-ref:c-let ((ode-rot %ode:real :from orot))
    (q! (ode-rot 0)
        (ode-rot 1)
        (ode-rot 2)
        (ode-rot 3))))

#+nil
(defun ode-geom-get-quaternion-from-mat3 (geom)
  (cffi-c-ref:c-let ((mrot %ode:matrix3 :from (%ode:geom-get-rotation geom)))
    #+nil
    (q:from-mat3 (m3:make (mrot 0)
                          (mrot 3)
                          (mrot 6)
                          ;;
                          (mrot 1)
                          (mrot 4)
                          (mrot 7)
                          ;;
                          (mrot 2)
                          (mrot 5)
                          (mrot 8)))
    ;;#+nil
    (q:from-mat3 (m3:make (mrot 0)
                          (mrot 1)
                          (mrot 2)
                          ;;
                          (mrot 3)
                          (mrot 4)
                          (mrot 5)
                          ;;
                          (mrot 6)
                          (mrot 7)
                          (mrot 8)))))

(defun getbody-linear-vel (body)
  (let ((linear-vel (%ode:body-get-linear-vel body)))
    (v! (cffi:mem-ref linear-vel :double 0)
        (cffi:mem-ref linear-vel :double 1)
        (cffi:mem-ref linear-vel :double 2))))

(defun getbody-angular-vel (body)
  (let ((linear-vel (%ode:body-get-angular-vel body)))
    (v! (cffi:mem-ref linear-vel :double 0)
        (cffi:mem-ref linear-vel :double 1)
        (cffi:mem-ref linear-vel :double 2))))

(defun getbody-force (body)
  (let ((force (%ode:body-get-force body)))
    (v! (cffi:mem-ref force :double 0)
        (cffi:mem-ref force :double 1)
        (cffi:mem-ref force :double 2))))

(defun getbody-force-and-torque (body)
  "debug helper"
  (let ((force (%ode:body-get-force body))
        (torque (%ode:body-get-torque body)))
    (values (v! (cffi:mem-ref force :double 0)
                (cffi:mem-ref force :double 1)
                (cffi:mem-ref force :double 2))
            (v! (cffi:mem-ref torque :double 0)
                (cffi:mem-ref torque :double 1)
                (cffi:mem-ref torque :double 2)))))
