(in-package #:scenic)

(defclass drawable ()
  ((drawp   :accessor drawp   :initarg :drawp   :documentation "should be drawn?")
   (shadowp :accessor shadowp :initarg :shadowp :documentation "casts shadow?"))
  (:default-initargs
   :drawp T
   :shadowp T))

(defclass actor (listener drawable)
  ((pos      :initarg :pos      :accessor pos      :documentation "3d position")
   (rot      :initarg :rot      :accessor rot      :documentation "3d rotation")
   (buf      :initarg :buf      :accessor buf      :documentation "buffer stream")
   (color    :initarg :color    :accessor color    :documentation "base color")
   (scale    :initarg :scale    :accessor scale    :documentation "vextex fudge scale")
   (material :initarg :material :accessor material :documentation "material index"))
  (:default-initargs
   :material 0
   :color (v! 1 1 1)
   :pos (v! 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box 1 1 1 t))
  (:documentation "base object, with tangents"))

(defclass assimp-thing (actor)
  ((albedo   :initarg :albedo)
   (normals  :initarg :normals)
   (specular :initarg :specular)
   (scene    :initarg :scene)))

(defclass assimp-thing-with-bones (actor)
  ((albedo   :initarg :albedo)
   (normals  :initarg :normals)
   (specular :initarg :specular)
   (scene    :initarg :scene)
   (bones    :initarg :bones
             :documentation "c-array of mat4s, of transforms for each bone in the whole scene")
   (bones-unique :reader bones-unique)
   (bones-transforms :reader bones-transforms)
   (scene-offset :reader scene-offset)
   (duration :initform 0f0 :initarg :duration)))


(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos) obj
      (format stream "(~a ~a ~a)" (x pos) (y pos) (z pos)))))

(defmethod specular  ((obj actor)) (specular  (nth (material obj) (materials *state*))))
(defmethod metallic  ((obj actor)) (metallic  (nth (material obj) (materials *state*))))
(defmethod emissive  ((obj actor)) (emissive  (nth (material obj) (materials *state*))))
(defmethod roughness ((obj actor)) (roughness (nth (material obj) (materials *state*))))

(defmethod (setf specular)  (new-value (obj actor)) (setf (specular  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf metallic)  (new-value (obj actor)) (setf (metallic  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf emissive)  (new-value (obj actor)) (setf (emissive  (nth (material obj) (materials *state*))) new-value))
(defmethod (setf roughness) (new-value (obj actor)) (setf (roughness (nth (material obj) (materials *state*))) new-value))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

(defmethod (setf pos) :before (new-value (obj actor))
  (unless (v3:= (slot-value obj 'pos) new-value)
    (let ((scene (current-scene)))
      (mapc (lambda (l) (setf (drawp l) T)) (lights scene))
      (when (typep scene 'scene-ibl)
        (setf (pos scene) (pos scene))))))

(defmethod update ((obj actor) dt))
