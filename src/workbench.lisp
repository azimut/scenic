(in-package #:scenic)

(let ((e (first (spot-lights (lights (current-scene))))))
  (defmethod update ((obj spot) dt)
    #+nil
    (when (eq e obj)
      (let* ((new-pos (god-move 2000 dt obj))
             (new-dis (v3:distance new-pos (v! 0 0 0))))
        (setf (far obj)  (+ new-dis (* new-dis .2)))
        (setf (near obj) (- new-dis (* new-dis .1)))
        ;;(setf (pos obj) new-pos)
        ;; (setf (cutoff obj)       (radians 13.5))
        ;; (setf (outer-cutoff obj) (radians 17.5))
        ;; (setf (linear obj)    (y (nth 8 *point-light-params*)))
        ;; (setf (quadratic obj) (z (nth 8 *point-light-params*)))
        ;;(setf (rot obj) (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
        ;;(setf (color obj) (v! .5 .4 .4))
        ))))

(let ((e (second (point-lights (lights (current-scene))))))
  (defmethod update ((obj point) dt)
    #+nil
    (when (eq e obj)
      (god-move 5000 dt obj))))

(defmethod update ((camera perspective) dt)
  (god-move 2000 dt camera)
  (full-rot 2000 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defun init-all-the-things ()
  (init-state
   (list (make-material :roughness .8 :metallic .02 :specular .1))
   (list
    (make-scene
     (list (make-perspective :scale .5 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
     (make-lights
      :dir-lights    nil
      :spot-lights  (list (make-spot  :pos (v! 4 4 4) :color (v! .8 .8 .8) :linear 0.35 :quadratic 0.44))
      :point-lights (list (make-point :pos (v! -2 2 -2) :color (v! .1 .1 .3) :linear 0.35 :quadratic 0.44)
                          (make-point :pos (v! 2 2 2) :color (v! .8 .2 .6) :linear 0.35 :quadratic 0.44)))
     (make-simple-postprocess))))
  ;; Actors
  (push (make-actor :w 10f0 :d 10f0) (actors (current-scene)))
  (dotimes (i 5)
    (let ((a (make-actor)))
      (setf (pos a) (v! (- (random 5) 2.5) 1 (- (random 5) 2.5)))
      (setf (rot a) (q:from-axis-angle (v! 0 1 0) (radians (random 180))))
      (push a (actors (current-scene)))))
  (push (make-actor :h 5f0) (actors (current-scene)))
  )

(defmethod update ((obj directional) dt)
  (let* ((new-pos (v3:*s (v! 20 10 -30) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    ;; (setf (pos obj) new-pos)
    ;; (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    ;; (setf (far obj)  (+ new-dis (* new-dis .1)))
    ;; (setf (near obj) (- new-dis (* new-dis .1)))
    ;; (setf (fs obj) (v2! 10))
    ))





