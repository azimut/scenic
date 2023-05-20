(in-package #:scenic)

;; NO idea how jointgroup is gonna be

(defvar *world* nil)
(defvar *space* nil)
(defvar *contactgroup* nil)

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
    (v! (cffi:mem-ref linear-vel :float 0)
        (cffi:mem-ref linear-vel :float 1)
        (cffi:mem-ref linear-vel :float 2))))

(defun getbody-angular-vel (body)
  (let ((linear-vel (%ode:body-get-angular-vel body)))
    (v! (cffi:mem-ref linear-vel :float 0)
        (cffi:mem-ref linear-vel :float 1)
        (cffi:mem-ref linear-vel :float 2))))

(defun getbody-force (body)
  (let ((force (%ode:body-get-force body)))
    (v! (cffi:mem-ref force :float 0)
        (cffi:mem-ref force :float 1)
        (cffi:mem-ref force :float 2))))

(defun getbody-force-and-torque (body)
  (let ((force (%ode:body-get-force body))
        (torque (%ode:body-get-torque body)))
    (values (v! (cffi:mem-ref force :float 0)
                (cffi:mem-ref force :float 1)
                (cffi:mem-ref force :float 2))
            (v! (cffi:mem-ref torque :float 0)
                (cffi:mem-ref torque :float 1)
                (cffi:mem-ref torque :float 2)))))
