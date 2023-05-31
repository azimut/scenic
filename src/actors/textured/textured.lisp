(in-package #:scenic)

(defclass textured (actor)
  ((albedo     :initarg :albedo)
   (normal     :initarg :normal)
   (roughmap   :initarg :roughmap)
   (specmap    :initarg :specmap)
   (aomap      :initarg :aomap)
   (uv-repeat  :initarg :uv-repeat))
  (:default-initargs
   :aomap    (get-tex "static/2k_wall/8/white_plaster_02_ao_2k.png"    NIL T :r8)
   :albedo   (get-tex "static/2k_wall/8/white_plaster_02_diff_2k.png"  NIL T :rgb8)
   :normal   (get-tex "static/2k_wall/8/white_plaster_02_nor_2k.png"   NIL T :rgb8)
   :specmap  (get-tex "static/2k_wall/8/white_plaster_02_spec_2k.png"  NIL T :r8)
   :roughmap (get-tex "static/2k_wall/8/white_plaster_02_rough_2k.png" NIL T :r8)
   :uv-repeat (v! 1 1)
   :buf (box 1f0 1f0 1f0 t)))

(defmethod initialize-instance :before ((obj textured) &key albedo normal roughmap aomap specmap)
  (check-type roughmap %cepl.types:sampler)
  (check-type albedo %cepl.types:sampler)
  (check-type normal %cepl.types:sampler)
  (check-type aomap %cepl.types:sampler)
  (check-type specmap %cepl.types:sampler))

(defun make-textured (&rest args)
  (apply #'make-instance 'textured args))