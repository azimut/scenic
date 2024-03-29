(in-package #:scenic)

(defvar *world* nil)
(defvar *contactgroup* nil)

(defclass ode-space ()
  ((space      :reader space)
   (stepper    :reader stepper :initform (make-stepper (seconds .05) (seconds .05)))
   (floor-geom :reader floor-geom))
  (:documentation "have a SCENE inherit from this, and send the SPACE to init geometries"))

(defmethod collide (o1 o2))

(defclass scene-ode (scene ode-space) ())
(defclass scene-ode-ibl (scene-ibl ode-space) ())

(defun make-scene-ode (&rest args)
  (apply #'make-instance 'scene-ode args))
(defun make-scene-ode-ibl (&rest args)
  (apply #'make-instance 'scene-ode-ibl args))

(defun ode-init-world ()
  (unless *world*
    (%ode:init-ode)
    (setf *world*        (%ode:world-create)
          *contactgroup* (%ode:joint-group-create 0)))
  (%ode:world-set-gravity *world* 0f0 -90.8f0 0f0)
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

(defun ode-stop-world ()
  (when (and *world* *contactgroup*)
    (%ode:joint-group-destroy *contactgroup*)
    (%ode:world-destroy *world*)
    (%ode:close-ode)
    (setf *world* nil
          *contactgroup* nil))
  t)

(defmethod initialize-instance :after ((obj ode-space) &key)
  (with-slots (space floor-geom) obj
    (setf space (%ode:hash-space-create nil))
    (setf floor-geom (%ode:create-plane space 0f0 1f0 0f0 0f0))))

(defmethod free :after ((obj ode-space))
  (%ode:space-destroy (space obj)))

(block gg
  (cffi:defcallback near-callback
      :void ((data :pointer) (g1 %ode:geom-id) (g2 %ode:geom-id))
    (declare (ignore data))
    (let ((b1 (%ode:geom-get-body g1))
          (b2 (%ode:geom-get-body g2)))
      (when (and (not (cffi:null-pointer-p b1))
                 (not (cffi:null-pointer-p b2))
                 (plusp (%ode:are-connected-excluding b1 b2 4 ;;%ode:+joint-type-contact+
                                                      )))
        (return-from gg))
      (cffi-c-ref:c-with ((contact %ode:contact :clear t :count 5))
        (dotimes (i 5)
          (setf (contact i :surface :mode)
                (logior %ode:+contact-bounce+
                        ;;%ode:+contact-slip1+
                        ;; %ode:+contact-slip2+
                        %ode:+contact-approx1+
                        %ode:+contact-soft-erp+
                        %ode:+contact-soft-cfm+
                        )
                ;;(contact i :surface :slip1) .7f0
                ;; (contact i :surface :slip2) .7f0
                ;; error reduction - spippery/cartoonish
                (contact i :surface :soft-erp) 0.4;; 0.1-0.6
                ;; friction parameter
                (contact i :surface :mu) ode:+infinity+
                ;;(contact i :surface :mu2) 0f0
                ;; bounce is the amount of "bouncyness"
                (contact i :surface :bounce) .1f0;; .1f0
                ;; bounce_vel is the minimum incoming velocity to cause a bounce
                (contact i :surface :bounce-vel) 0.1;;.1f0
                ;; constraint force mixing parameter, soft/bouncy
                (contact i :surface :soft-cfm) 0.01;; .01f0 - 0.5
                ))
        (let ((numc (%ode:collide g1 g2 5 (contact :geom &) (cffi:foreign-type-size '%ode:contact))))
          (when (plusp numc)
            (when (and b1 b2)
              )
            (dotimes (i numc)
              (%ode:joint-attach
               (%ode:joint-create-contact *world* *contactgroup* (contact i))
               b1 b2))))))))

(defmethod handle :after ((e tick) (obj ode-space))
  (with-slots (space stepper) obj
    (%ode:space-collide space nil (cffi:callback near-callback))
    (%ode:world-quick-step *world* 0.01f0)
    (%ode:joint-group-empty *contactgroup*)))
