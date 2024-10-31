(in-package #:scenic)

(defclass actor (listener drawable occluder paintable)
  ((name     :initarg :name     :reader   name)
   (pos      :initarg :pos      :accessor pos      :documentation "3d position")
   (rot      :initarg :rot      :accessor rot      :documentation "3d rotation")
   (buf      :initarg :buf      :accessor buf      :documentation "buffer stream")
   (color    :initarg :color    :accessor color    :documentation "base color")
   (scale    :initarg :scale    :accessor scale    :documentation "vextex fudge scale")
   (material :initarg :material :accessor material :documentation "material index"))
  (:default-initargs
   :name (gensym)
   :material 0
   :color (v! 1 1 1)
   :pos (v! 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box 1 1 1 t))
  (:documentation "base object, with tangents"))

(defclass assimp-thing (actor)
  ((scene :initarg :scene)))

(defclass assimp-thing-with-bones (assimp-thing)
  ((bones            :initarg :bones
                     :documentation
                     "c-array of mat4s, of transforms for each bone in the whole scene")
   (bones-unique     :reader bones-unique)
   (bones-transforms :reader bones-transforms)
   (scene-offset     :reader scene-offset)
   (duration         :initform 0f0 :initarg :duration)))

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos) obj
      (format stream "(~$ ~$ ~$)" (x pos) (y pos) (z pos)))))

(defmethod print-object ((obj assimp-thing) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos name) obj
      (format stream "(~$ ~$ ~$) ~s"
              (x pos) (y pos) (z pos)
              name))))

;; Accessors
(defmethod specular  ((obj actor))
  (specular  (nth (material obj) (materials *state*))))
(defmethod metallic  ((obj actor))
  (metallic  (nth (material obj) (materials *state*))))
(defmethod emissive  ((obj actor))
  (emissive  (nth (material obj) (materials *state*))))
(defmethod roughness ((obj actor))
  (roughness (nth (material obj) (materials *state*))))

;; Setters
(defmethod (setf specular) (new-value (obj actor))
  (setf (specular  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf metallic) (new-value (obj actor))
  (setf (metallic  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf emissive) (new-value (obj actor))
  (setf (emissive  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf roughness) (new-value (obj actor))
  (setf (roughness (nth (material obj) (materials *state*))) new-value))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

(defmethod (setf pos) :before (new-value (obj actor))
  (when (not (v3:= (slot-value obj 'pos) new-value))
    (issue (current-scene) 'movement)))

(defmethod (setf rot) :before (new-value (obj actor))
  (when (not (q:= (slot-value obj 'rot) new-value))
    (issue (current-scene) 'movement)))

(defmethod (setf scale) :before (new-value (obj actor))
  (when (not (= (slot-value obj 'scale) new-value))
    (issue (current-scene) 'movement)))

(defmethod update ((obj actor) dt))
