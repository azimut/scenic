(in-package #:scenic)

(defclass derived (physic)
  (ode-vertices
   ode-indices
   data)
  (:default-initargs
   :buf (error "needs a BUF to convert it"))
  (:documentation "derives an object with a BUF slot into an ODE object"))

(defmethod initialize-instance :after ((obj derived) &key buf body mass density immovablep space)
  (with-slots (ode-vertices ode-indices data geom) obj
    (multiple-value-bind (v i d g) (buffer-stream-to-ode buf space)
      (setf ode-vertices v
            ode-indices  i
            data         d
            geom         g))
    (unless immovablep
      (%ode:geom-set-data     geom data)
      (%ode:mass-set-trimesh  mass density geom)
      (%ode:geom-set-position geom 0f0 0f0 0f0)
      (%ode:mass-translate    mass 0f0 0f0 0f0)
      (%ode:geom-set-body     geom body)
      (%ode:body-set-mass     body mass))))

(defun buffer-stream-to-ode (buf space)
  (destructuring-bind ((gv) gi) (buffer-stream-gpu-arrays buf)
    (let ((gvl  (car (gpu-array-dimensions gv)))
          (gil  (car (gpu-array-dimensions gi)))
          (data (%ode:geom-tri-mesh-data-create)))

      (cffi-c-ref:c-let ((vertices :float        :alloc t :count (* 3 gvl))
                         (indices  :unsigned-int :alloc t :count gil))

        (with-gpu-array-as-c-array (ci gi)
          (loop :for i :below gil
                :do (setf (indices i) (aref-c ci i))))

        (with-gpu-array-as-c-array (cv gv)
          (loop :for i :below gvl
                :for ovx :by 3
                :for ovy := (+ ovx 1)
                :for ovz := (+ ovx 2)
                :do (setf (vertices ovx) (x (pos (aref-c cv i))))
                    (setf (vertices ovy) (y (pos (aref-c cv i))))
                    (setf (vertices ovz) (z (pos (aref-c cv i))))))

        (%ode:geom-tri-mesh-data-build-single
         data
         (vertices &)     (* 3 (cffi:foreign-type-size :float)) gvl
         (indices  &) gil (* 3 (cffi:foreign-type-size :unsigned-int)))

        (values (vertices &)
                (indices &)
                data
                (%ode:create-tri-mesh space data
                                      (cffi:null-pointer)
                                      (cffi:null-pointer)
                                      (cffi:null-pointer)))))))

(defmethod free :after ((object derived))
  (with-slots (data ode-vertices ode-indices) object
    (cffi:foreign-free data)
    (cffi:foreign-free ode-indices)
    (cffi:foreign-free ode-vertices)))

