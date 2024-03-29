(in-package #:scenic)

;; This became a big hack/escaping hatch...but I am ok with it :)
;; I guess in a way it was the first iteration of an event loop.

(defgeneric register (unit container))

(defmethod register :after (unit (scene scene))
  (add-listener unit scene))

(defmethod register ((actor actor) scene)
  (issue scene 'movement)
  (push actor (actors scene)))

(defmethod register ((actor particles) scene)
  (push actor (actors scene)))

(defmethod register ((camera defered) scene)
  (push camera (cameras scene)))
(defmethod register ((camera perspective) scene)
  (push camera (cameras scene)))
(defmethod register ((camera orthogonal) scene)
  (push camera (cameras scene)))

;; TODO: keep directional light first, due assumptions made on fog/sky
(defmethod register ((light light) scene)
  (with-slots (idx fbo) light
    (flet ((same-light (x) (typep x (class-name-of light))))
      (init-light light (length (remove-if-not #'same-light (lights scene))))))
  (push light (lights scene)))

(defmethod register :around ((light directional) scene)
  (with-gpu-array-as-c-array (c (ubo-data (ubo scene)))
    (when (< (scene-data-ndir (aref-c c 0)) 2)
      (incf (scene-data-ndir (aref-c c 0)))
      (call-next-method))))
(defmethod register :around ((light point) scene)
  (with-gpu-array-as-c-array (c (ubo-data (ubo scene)))
    (when (< (scene-data-npoint (aref-c c 0)) 4)
      (incf (scene-data-npoint (aref-c c 0)))
      (call-next-method))))
(defmethod register :around ((light spot) scene)
  (with-gpu-array-as-c-array (c (ubo-data (ubo scene)))
    (when (< (scene-data-nspot (aref-c c 0)) 2)
      (incf (scene-data-nspot (aref-c c 0)))
      (call-next-method))))

(defmethod register ((post postprocess) (scene scene))
  (when (not (find post (post scene)))
    (push post (post scene))))

(defmethod register ((scene scene) (state state))
  (add-listener state scene)
  (push scene (scenes state))
  (incf (slot-value state 'scene-index)))
