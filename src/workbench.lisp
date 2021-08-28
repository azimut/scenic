(in-package #:scenic)

(let ((e (second (actors (current-scene)))))
  (defmethod update ((obj actor) dt)
    ;;(full-rot 1000 dt obj)
    ;;(setf (rot obj) (q:identity))
    #+nil
    (when (eq e obj)
      ;;(cloud::upload-source cloud::*engine*)
      (setf (cloud::pos cloud::*engine*) (copy-seq (pos obj))))))

(let ((e (first (lights (current-scene)))))
  (defmethod update ((obj spot) dt)
    #+nil
    (when (eq e obj)
      ;;#+nil
      (let* ((new-pos (god-move 2000 dt obj))
             (new-dis (v3:distance new-pos (v! 0 0 0))))
        ;; (setf (far obj)  (+ new-dis (* new-dis .2)))
        ;; (setf (near obj) (- new-dis (* new-dis .1)))
        ;;(setf (pos obj) new-pos)
        ;; (setf (cutoff obj)       (radians 13.5))
        ;; (setf (outer-cutoff obj) (radians 17.5))
        ;; (setf (linear obj)    (y (nth 8 *point-light-params*)))
        ;; (setf (quadratic obj) (z (nth 8 *point-light-params*)))
        (setf (rot obj) (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
        ;;(setf (color obj) (v! 1 1 1))
        ))))

(let ((e (second (lights (current-scene)))))
  (defmethod update ((obj point) dt)
    #+nil
    (when (eq e obj)
      (god-move 5000 dt obj))))

(defun cloud:get-listener-pos () (pos (current-camera)))
(defun cloud-get-listener-rot () (rot (current-camera)))

(defmethod update ((camera perspective) dt)
  (god-move 2000 dt camera)
  (full-rot 2000 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defmethod update ((camera defered) dt)
  (god-move 2000 dt camera)
  (full-rot 2000 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defun match-camera ()
  (let* ((next-idx (mod (1+ (scene-index *state*)) (length (scenes *state*))))
         (other-cam (active-camera (nth next-idx (scenes *state*)))))
    (setf (pos other-cam) (copy-seq (pos (current-camera))))
    (setf (rot other-cam) (copy-seq (rot (current-camera))))))

(defvar *retention* (make-hash-table))
;; SETF only if the value is different

(defun setf-if-new (comparator old-value new-value &body body)
  (unless (funcall comparator old-value new-value)
    body))


(defmethod scene-action ((scene (eql 0))))

(defmethod update ((camera perspective) dt)
  (when (key-down-p key.j) (rocketman::next-row *rocket*))
  (when (key-down-p key.k) (rocketman::prev-row *rocket*))
  (when (key-down-p key.space) (rocket-pause-toggle))
  (setf (fov camera) (rocket-get "camera:fov"))
  (setf (pos camera) (rocket-get-v3 "camera:x" "camera:y" "camera:z"))
  (let ((new (q:point-at (v! 0 1 0) (pos camera)
                         (rocket-get-v3 "view:x" "view:y" "view:z"))))
    (unless (q:= (rot camera) new)
      (setf (rot camera) new)))
  ;;
  (let ((light (first (lights (current-scene))))
        (new-value (rocket-get-v3 "point1:x" "point1:y" "point1:z")))
    (unless (v3:= new-value (pos light))
      (setf (pos light) new-value)))
  ;; (god-move 2000 dt camera)
  ;; (full-rot 2000 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defun init-all-the-things ()
  (init-state
   (list (make-material :roughness .8 :metallic .02 :specular .1)))
  #+nil
  (push
   (make-scene
    (list (make-defered :scale .5 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
    (list (make-point :pos (v! -2 2 -2) :color (v! .1 .1 .3) :linear 0.35 :quadratic 0.44)
          (make-point :pos (v! 2 2 2) :color (v! .8 .2 .6) :linear 0.35 :quadratic 0.44))
    (make-defer-postprocess)
    :color (v! .3 .3 .2 1))
   (scenes *state*))
  (push
   (make-scene
    (list (make-perspective :scale .5 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
    (list (make-point :pos (v! -2 2 -2) :color (v! .1 .1 .3) :linear 0.35 :quadratic 0.44)
          (make-point :pos (v! 2 2 2) :color (v! .8 .2 .6) :linear 0.35 :quadratic 0.44))
    (make-simple-postprocess))
   (scenes *state*))
  ;;
  #+nil
  (push
   (make-scene
    (list (make-perspective :scale .5 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
    (list (make-spot  :pos (v! 4 4 4) :color (v! .8 .8 .8) :linear 0.35 :quadratic 0.44))
    (make-simple-postprocess))
   (scenes *state*))
  ;; Actors
  (let ((scene (first (scenes *state*))))
    (push (make-box :w 10f0 :d 10f0) (actors scene))
    (push (make-box :h 5f0) (actors scene)))
  #+nil
  (let ((scene (second (scenes *state*))))
    (push (make-box :w 10f0 :d 10f0) (actors scene))
    (push (make-box :h 5f0) (actors scene)))
  #+nil
  (let ((scene (second (scenes *state*))))
    (dotimes (i 2)
      (let ((a (make-sphere)))
        (setf (pos a) (v! (- (random 5) 2.5) 1 (- (random 5) 2.5)))
        (setf (rot a) (q:from-axis-angle (v! 0 1 0) (radians (random 180))))
        (push a (actors scene))))
    (dotimes (i 2)
      (let ((a (make-cone)))
        (setf (pos a) (v! (- (random 5) 2.5) 1 (- (random 5) 2.5)))
        (setf (rot a) (q:from-axis-angle (v! 0 1 0) (radians (random 180))))
        (push a (actors scene))))
    (push (make-box :w 10f0 :d 10f0) (actors scene))))

(defmethod update ((obj directional) dt)
  (let* ((new-pos (v3:*s (v! 20 10 -30) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    ;; (setf (pos obj) new-pos)
    ;; (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    ;; (setf (far obj)  (+ new-dis (* new-dis .1)))
    ;; (setf (near obj) (- new-dis (* new-dis .1)))
    ;; (setf (fs obj) (v2! 10))
    ))

