(in-package #:scenic)

(defvar *max-bones-per-vertex* 4)

(serapeum:-> list-bones (ai:scene) cons)
(defun list-bones (scene)
  "returns a plain list with all the bones in SCENE"
  (let* ((meshes (coerce (ai:meshes scene) 'list))
         (bones  (mappend (lambda (m) (coerce (ai:bones m) 'list)) meshes))
         (bones  (remove NIL bones)))
    bones))

(serapeum:-> list-bones-unique (ai:scene) cons)
(defun list-bones-unique (scene)
  "returns a plain list with all unique bones in SCENE"
  (serapeum:~>
   (list-bones scene)
   (remove-duplicates :key #'ai:name :test #'string=)))

;;--------------------------------------------------
;; Bones loader

(defun get-bones-per-vertex (scene mesh-bones n-vertices)
  "Returns an array OF lists OF conses. With each vertex, (BONE-ID . WEIGHT) info.
   To be used once on the buffer stream load. It uses the bones on the meshes
   to build the BONE-IDs because there could be meshes without animation but
   with bones.
   ex: #(((1 . .9) (2 . .1)) ((10 . .2) (20 . .8)))"
  (declare (type ai:scene        scene)
           (type vector          mesh-bones)
           (type positive-fixnum n-vertices))
  (let ((unique-scene-bones (list-bones-unique scene))
        (v-to-bones (make-array n-vertices :initial-element NIL)))
    (loop :for bone :across mesh-bones
          :for bone-id := (position (ai:name bone) unique-scene-bones
                                    :test #'string=
                                    :key  #'ai:name)
          :do (assert bone-id)
              (loop :for weight :across (ai:weights bone) :do
                (with-slots ((v ai:id) (w ai:weight)) weight
                  (when (and (>= w .1) ;; discard bones with low influence
                             (< (length (aref v-to-bones v))
                                *max-bones-per-vertex*))
                    (push (cons bone-id w)
                          (aref v-to-bones v))
                    ;; Sort descending by weights
                    (setf (aref v-to-bones v)
                          (sort (aref v-to-bones v) #'>
                                :key #'cdr))))))
    v-to-bones))

;;--------------------------------------------------
;; Bone animation helpers

(serapeum:-> find-index (number vector) fixnum)
(defun find-index (etime positions)
  "returns the index position matching the current ETIME"
  (let ((pos (position-if (lambda (p) (< etime (slot-value p 'time)))
                          positions)))
    (if pos
        (max 0 (1- pos))
        0)))

(serapeum:-> calc-interpolated-position (number vector) rtg-math.types:vec3)
(defun calc-interpolated-position (etime positions)
  (declare (type vector positions))
  (if (or (< etime (slot-value (aref positions 0) 'time))
          (length= 1 positions))
      (ai:value (aref positions 0))
      (let* ((index       (find-index etime positions))
             (next-index  (1+ index))
             (current-pos (aref positions index))
             (next-pos    (aref positions next-index)))
        (with-slots ((ctime time) (start ai:value)) current-pos
          (with-slots ((ntime time) (end ai:value)) next-pos
            (let* ((dt     (- ntime ctime))
                   (factor (/ (- etime ctime) dt))
                   (delta  (v3:- end start)))
              (v3:+ start
                    (v3:*s delta (coerce factor 'single-float)))))))))

(serapeum:-> calc-interpolated-rotation (number vector) rtg-math.types:quaternion)
(defun calc-interpolated-rotation (etime rotations)
  (if (or (< etime (slot-value (aref rotations 0) 'time))
          (length= 1 rotations))
      (ai:value (aref rotations 0))
      (let* ((index       (find-index etime rotations))
             (next-index  (1+ index))
             (current-rot (aref rotations index))
             (next-rot    (aref rotations next-index)))
        (with-slots ((ctime time) (start ai:value)) current-rot
          (with-slots ((ntime time) (end ai:value)) next-rot
            (let* ((dt      (- ntime ctime))
                   (factor  (/ (- etime ctime) dt))
                   (qinterp (q:lerp start end (coerce factor 'single-float))))
              (q:normalize qinterp)))))))

(serapeum:-> get-frame-transform (ai::node-animation fixnum) rtg-math.types:mat4)
(defun get-frame-transform (node-animation frame)
  "returns a matrix"
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    ;; reset frame position
    (let ((reset-frame (mod frame (length rot-keys))))
      ;; Calculate tranform matrix based on time
      (m4-n:*
       (m4:translation (ai:value (aref pos-keys reset-frame)))
       (q:to-mat4      (ai:value (aref rot-keys reset-frame)))
       ;;(m4:scale (v3! 1.6))
       ;;(m4:scale (ai:value (aref sca-keys 0)))
       ))))

(serapeum:-> get-time-transform (ai::node-animation number) rtg-math.types:mat4)
(defun get-time-transform (node-animation time)
  "calculate the tansform matrix based on time returns a matrix"
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    (let ((ipos (calc-interpolated-position time pos-keys))
          (irot (calc-interpolated-rotation time rot-keys)))
      (m4-n:*
       (m4:translation ipos)
       (q:to-mat4 irot)
       ;;(m4:scale (v3! .9f0))
       ;;(m4:scale (ai:value (aref sca-keys 0)))
       ))))

(defgeneric get-nodes-transforms (scene node-type &key frame time nth-animation)
  (:documentation "returns a hash of mat4's with each node transform
    for value and node name for the key")
  (:method ((scene ai:scene) (node-type (eql :static)) &key frame time nth-animation)
    (let ((nodes-transforms (make-hash-table :test #'equal)))
      (labels ((walk-node (node parent-transform)
                 (declare (type ai:node node)
                          (type vector parent-transform))
                 (with-slots ((name      ai:name)
                              (transform ai:transform)
                              (children  ai:children))
                     node
                   (let ((global
                           (m4:* parent-transform (m4:transpose transform))))
                     (setf (gethash name nodes-transforms) global)
                     (map 'vector
                          (lambda (c) (walk-node c global))
                          children)))))
        (walk-node (ai:root-node scene) (m4:identity)))
      nodes-transforms))
  (:method ((scene ai:scene) (node-type (eql :animated)) &key frame time (nth-animation 0))
    (let* ((animation        (aref (ai:animations scene) nth-animation))
           (duration         (ai:duration animation))
           ;; NOTE: animation-index is a hash lookup table for BONE>NODE-ANIMATION
           (animation-index  (ai:index animation))
           ;; NOTE: temporal lookup table for bones *thinking emoji*
           (nodes-transforms (make-hash-table :test #'equal)))
      (declare (type hash-table animation-index))
      (labels
          ((walk-node (node parent-transform)
             (declare (type ai:node node) (type vector parent-transform))
             (with-slots ((name      ai:name)
                          (transform ai:transform)
                          (children  ai:children))
                 node
               ;; FIXME: see below mess
               (let* ((node-anim (gethash name animation-index))
                      (time-transform
                        (when node-anim
                          (if frame
                              (get-frame-transform node-anim frame)
                              (get-time-transform  node-anim (mod time duration)))))
                      (final-transform (if time-transform
                                           time-transform
                                           (m4:transpose transform)))
                      (global (m4:* parent-transform final-transform)))
                 (setf (gethash name nodes-transforms) global)
                 ;; WALK!
                 (map 'vector
                      (lambda (c) (walk-node c global))
                      children)))))
        (walk-node (ai:root-node scene)
                   (m4:identity)))
      nodes-transforms)))

(defun get-bones-tranforms (scene &key (frame 0 frame-p) (time 0 time-p))
  (declare (ai:scene scene))
  (let* ((root-offset (-> scene
                          (ai:root-node)
                          (ai:transform)
                          (m4:transpose)
                          (m4:inverse)))
         (unique-bones     (list-bones-unique scene))
         (bones-transforms (make-array (length unique-bones)))
         ;; NOTE: It might have bones but NO animation
         (node-type        (cond ((and frame-p time-p)
                                  (error "provide EITHER time or frame offset"))
                                 ((emptyp (ai:animations scene))
                                  :static)
                                 ((not (emptyp (ai:animations scene)))
                                  :animated)
                                 (t (error "cannot figure out boned mesh type"))))
         (nodes-transforms (if frame-p
                               (get-nodes-transforms scene node-type
                                                     :frame frame)
                               (get-nodes-transforms scene node-type
                                                     :time time))))
    (declare (type hash-table nodes-transforms))
    (loop :for bone :in unique-bones
          :for bone-id :from 0
          :do (with-slots ((name   ai:name)
                           (offset ai:offset-matrix))
                  bone
                (let ((node-transform (gethash name nodes-transforms)))
                  (setf (aref bones-transforms bone-id)
                        ;; I got a mesh that has 0 on the bones offsets...
                        ;; The mesh also didn't have animations so might be
                        ;; that was the reason...
                        ;;node-transform
                        (if (m4:0p offset)
                            (m4:* root-offset
                                  node-transform)
                            (m4:* root-offset
                                  node-transform
                                  (m4:transpose offset)))))))
    bones-transforms))

(fare-memoization:define-memo-function get-bones-time-tranforms
    (scene nth-animation time)
  (declare (ai:scene scene))
  (let* ((root-offset (-> scene
                          (ai:root-node)
                          (ai:transform)
                          (m4:transpose)
                          (m4:inverse)))
         (unique-bones     (list-bones-unique scene))
         (bones-transforms (make-array (length unique-bones)))
         ;; NOTE: It might have bones but NO animation
         (node-type        (if (emptyp (ai:animations scene))
                               :static
                               :animated))
         (nodes-transforms (get-nodes-transforms scene node-type :time time :nth-animation nth-animation)))
    (declare (type hash-table nodes-transforms))
    (loop :for bone :in unique-bones
          :for bone-id :from 0
          :do (with-slots ((name   ai:name)
                           (offset ai:offset-matrix))
                  bone
                (let ((node-transform (gethash name nodes-transforms)))
                  (setf (aref bones-transforms bone-id)
                        ;; I got a mesh that has 0 on the bones offsets...
                        ;; The mesh also didn't have animations so might be
                        ;; that was the reason...
                        ;;node-transform
                        (if (m4:0p offset)
                            (m4:* root-offset
                                  node-transform)
                            (m4:* root-offset
                                  node-transform
                                  (m4:transpose offset)))))))
    bones-transforms))
