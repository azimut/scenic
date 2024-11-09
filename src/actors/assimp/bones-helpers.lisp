(in-package #:scenic)

(defvar *max-bones-per-vertex* 4)

(s:-> list-bones (ai:scene) cons)
(defun list-bones (scene)
  "returns a plain list with all the bones in SCENE"
  (let* ((meshes (coerce (ai:meshes scene) 'list))
         (bones  (mappend (lambda (m) (coerce (ai:bones m) 'list)) meshes))
         (bones  (remove NIL bones)))
    bones))

(s:-> list-bones-unique (ai:scene) cons)
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
   ex: #((( 1 . 0.9) ( 2 . 0.1))
         ((10 . 0.2) (20 . 0.8)))"
  (declare (type ai:scene        scene)
           (type vector          mesh-bones)
           (type positive-fixnum n-vertices))
  (let ((v-to-bones (make-array n-vertices :initial-element NIL)))
    (loop :with unique-scene-bones = (list-bones-unique scene)
          :for bone :across mesh-bones
          :for bone-id = (position (ai:name bone) unique-scene-bones
                                   :test #'string=
                                   :key  #'ai:name)
          :do (assert bone-id)
              (loop :for weight :across (ai:weights bone) :do
                (with-slots ((bid ai:id) (w ai:weight)) weight
                  (when (<= 0 w 1) ;; discard bones with wacky weight (paranoid!)
                    (push (cons bone-id w) (aref v-to-bones bid))))))
    ;; Sort descending by weights and keep length under max
    (map 'vector (lambda (v2b) (s:take *max-bones-per-vertex* (sort v2b #'> :key #'cdr)))
         v-to-bones)))

;;--------------------------------------------------
;; Bone animation helpers

(s:-> find-index (number vector) fixnum)
(defun find-index (etime positions)
  "returns the index position matching the current ETIME"
  (let ((pos (position-if (lambda (p) (< etime (slot-value p 'time)))
                          positions)))
    (if pos
        (max 0 (1- pos))
        0)))

(s:-> calc-interpolated-position (number vector) rtg-math.types:vec3)
(defun calc-interpolated-position (etime positions &aux (start-time (slot-value (aref positions 0) 'time)))
  (if (or (< etime start-time) (length= 1 positions))
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

(s:-> calc-interpolated-rotation (number vector) rtg-math.types:quaternion)
(defun calc-interpolated-rotation (etime rotations &aux (start-time (slot-value (aref rotations 0) 'time)))
  (if (or (< etime start-time) (length= 1 rotations))
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

(s:-> calc-interpolated-scale (number vector) rtg-math.types:vec3)
(defun calc-interpolated-scale (etime scales &aux (start-time (slot-value (aref scales 0) 'time)))
  (if (or (< etime start-time) (length= 1 scales))
      (ai:value (aref scales 0))
      (let* ((index       (find-index etime scales))
             (next-index  (1+ index))
             (current-pos (aref scales index))
             (next-pos    (aref scales next-index)))
        (with-slots ((ctime time) (start ai:value)) current-pos
          (with-slots ((ntime time) (end ai:value)) next-pos
            (let* ((dt     (- ntime ctime))
                   (factor (/ (- etime ctime) dt))
                   (delta  (v3:- end start)))
              (v3:+ start
                    (v3:*s delta (coerce factor 'single-float)))))))))

(s:-> get-time-transform (ai::node-animation number) rtg-math.types:mat4)
(defun get-time-transform (node-animation time)
  "calculate the tansform matrix based on time returns a matrix"
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    (let ((ipos (calc-interpolated-position time pos-keys))
          (irot (calc-interpolated-rotation time rot-keys))
          (isca (calc-interpolated-scale    time sca-keys)))
      (m4-n:* ;; FIXME: Here is a vec3. And we use 1 single float.
       (m4:translation ipos)
       (q:to-mat4      irot)
       (m4:scale       isca)))))

(s:-> get-static-bone-transforms (ai:scene) hash-table)
(defun get-static-bone-transforms (scene)
  "returns a hash of mat4's:
   - key being the node/bone name
   - value the node transform"
  (s:lret ((nodes-transforms (make-hash-table :test #'equal)))
    (labels ((walk-node (node parent-transform)
               (declare (type ai:node node) (type vector parent-transform))
               (with-slots ((name ai:name) (transform ai:transform) (childrens ai:children))
                   node
                 (let ((new-transform (m4:* parent-transform (m4:transpose transform))))
                   (setf (gethash name nodes-transforms) new-transform)
                   (loop :for children :across childrens :do
                     (walk-node children new-transform))))))
      (walk-node (ai:root-node scene) (m4:identity)))))

(s:-> get-animated-bone-transforms (ai:scene number fixnum) hash-table)
(defun get-animated-bone-transforms (scene time nth-animation)
  "returns a hash of mat4's:
   - key being the node/bone name
   - value the node transform at TIME"
  (s:lret ((nodes-transforms (make-hash-table :test #'equal))) ;; NODE->TRANSFORM
    (with-slots ((duration ai:duration) (animation-index ai:index)) ;; BONE->NODE-ANIMATION
        (aref (ai:animations scene) nth-animation)
      (declare (type hash-table animation-index nodes-transform))
      (labels ((walk-node (node parent-transform)
                 (declare (type ai:node node) (type vector parent-transform))
                 (with-slots ((name ai:name) (old-transform ai:transform) (childrens ai:children))
                     node
                   (let ((new-transform
                           (m4:* parent-transform
                                 ;; if it's NOT part of the animation, do nothing (m4:identity)
                                 (a:if-let ((node-anim (gethash name animation-index)))
                                   (get-time-transform node-anim (mod time duration))
                                   (m4:identity)))))

                     (setf (gethash name nodes-transforms) new-transform)

                     (loop :for children :across childrens :do
                       (walk-node children new-transform))))))
        (walk-node (ai:root-node scene)
                   (m4:identity))))))

(s:-> get-bones-transforms (ai:scene number) simple-vector)
(defun get-bones-transforms (scene time)
  "Returns the final array of bones transformations.
   Creates the hashmap of the hierarchical bone transformations at TIME,
   and applies the bone offset."
  (let ((nodes-transforms
          (if (emptyp (ai:animations scene))
              (get-static-bone-transforms scene)
              (get-animated-bone-transforms scene time 0)));; FIXME: nth-animation
        (unique-bones
          (list-bones-unique scene)))
    (declare (type hash-table nodes-transforms))
    (s:lret ((bones-transforms (make-array (length unique-bones))))
      (loop :for bone :in unique-bones
            :for bone-id :from 0
            :do (with-slots ((name ai:name) (offset ai:offset-matrix)) bone
                  (let ((node-transform (gethash name nodes-transforms)))
                    (setf (aref bones-transforms bone-id)
                          ;; I got a mesh that has 0 on the bones offsets...
                          ;; The mesh also didn't have animations so might be
                          ;; that was the reason...
                          (if (m4:0p offset)
                              node-transform
                              (m4:* node-transform (m4:transpose offset))))))))))
