(in-package #:scenic)

(defclass albedoed ()
  ((albedo  :initarg :albedo)
   (gamma-p :initarg :gamma;; TODO
            :accessor gamma-p
            :documentation "whether apply gamma correction or not"))
  (:default-initargs
   :albedo (get-tex "static/null/2k_wall/8/white_plaster_02_diff_2k.png" NIL T :rgb8)
   :gamma-p T))

(defmethod initialize-instance :before ((obj albedoed) &key albedo gamma-p)
  (check-type albedo %cepl.types:sampler)
  (check-type gamma-p boolean))

(defmethod (setf gamma-p) :after (new-value (obj albedoed))
  (check-type new-value boolean))

(defclass normaled ()
  ((normal :initarg :normal)
   (flip-p :initarg :flip-p;; TODO
           :accessor flip-p
           :documentation "whether flip the normals or not"))
  (:default-initargs
   :flip-p NIL
   :normal (get-tex "static/null/2k_wall/8/white_plaster_02_nor_2k.png" NIL T :rgb8)))

(defmethod initialize-instance :before ((obj normaled) &key normal flip-p)
  (check-type normal %cepl.types:sampler)
  (check-type flip-p boolean))

(defmethod (setf flip-p) :before (new-value (obj normaled))
  (check-type new-value boolean))

(defclass displaced ()
  ((dispmap   :initarg :dispmap)
   (dispscale :accessor dispscale
              :initarg :dispscale))
  (:default-initargs
   :dispmap (get-tex "static/null/2k_wall/8/white_plaster_02_disp_2k.png" NIL T :r8)
   :dispscale .01))

(defmethod initialize-instance :before ((obj displaced) &key dispscale dispmap)
  (check-type dispmap %cepl.types:sampler)
  (check-type dispscale single-float))

(defmethod (setf dispscale) :before (new-value (obj displaced))
  (check-type new-value single-float))

(defclass roughed ()
  ((roughmap :initarg :roughmap))
  (:default-initargs
   :roughmap (get-tex "static/null/2k_wall/8/white_plaster_02_rough_2k.png" NIL T :r8)))

(defmethod initialize-instance :before ((obj roughed) &key roughmap)
  (check-type roughmap %cepl.types:sampler))

(defclass aoded ()
  ((aomap :initarg :aomap))
  (:default-initargs
   :aomap (get-tex "static/null/2k_wall/8/white_plaster_02_ao_2k.png" NIL T :r8)))

(defmethod initialize-instance :before ((obj aoded) &key aomap)
  (check-type aomap %cepl.types:sampler))

(defclass speced ()
  ((specmap :initarg :specmap))
  (:default-initargs
   :specmap (get-tex "static/null/2k_wall/8/white_plaster_02_spec_2k.png" NIL T :r8)))

(defmethod initialize-instance :before ((obj speced) &key specmap)
  (check-type specmap %cepl.types:sampler))

;;----------------------------------------

(defclass textured ()
  ((uv-repeat :accessor uv-repeat
              :initarg :uv-repeat))
  (:default-initargs
   :uv-repeat (v! 1 1)))

(defmethod initialize-instance :before ((obj textured) &key uv-repeat)
  (check-type uv-repeat rtg-math.types:vec2))

(defmethod (setf uv-repeat) :before (new-value (obj textured))
  (check-type new-value rtg-math.types:vec2))

;;----------------------------------------

(defclass textured-pbr (textured actor albedoed normaled displaced roughed aoded speced)
  ()
  (:default-initargs
   :buf (box 1f0 1f0 1f0 t))) ; It needs tangents for PBR
