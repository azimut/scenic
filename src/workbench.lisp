(in-package #:scenic)

(defmethod update ((camera perspective) dt)
  (control camera dt 2000)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defun init-all-the-things ()
  (init-state
   (list (make-pbr :roughness .8 :metallic .02 :specular .1))
   (list
    (make-scene
     (list (make-perspective :scale .5 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
     (make-lights
      :dir-lights nil
      :point-lights
      (list (make-point :pos (v! -2 2 -2) :color (v! .1 .1 .3) :linear 0.35 :quadratic 0.44)
            (make-point :pos (v! 2 2 2) :color (v! .8 .2 .6) :linear 0.35 :quadratic 0.44)))
     (make-simple-postprocess))))
  ;; Actors
  (push (make-actor :w 10f0 :d 10f0) (actors (current-scene)))
  (dotimes (i 5)
    (let ((a (make-actor)))
      (setf (pos a) (v! (- (random 5) 2.5) 1 (- (random 5) 2.5)))
      (setf (rot a) (q:from-axis-angle (v! 0 1 0) (radians (random 180))))
      (push a (actors (current-scene)))))
  (push (make-actor :h 5f0) (actors (current-scene))))

(defmethod update ((obj directional) dt)
  (let* ((new-pos (v3:*s (v! 20 10 -30) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    ;; (setf (pos obj) new-pos)
    ;; (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    ;; (setf (far obj)  (+ new-dis (* new-dis .1)))
    ;; (setf (near obj) (- new-dis (* new-dis .1)))
    ;; (setf (fs obj) (v2! 10))
    ))

(defmethod update ((obj point) dt)
  ;;#+nil
  (let* ((new-pos (v! 3 4 2)))
    ;; (setf (pos obj) new-pos)
    ;; (setf (far obj)  10f0)
    ;; (setf (near obj) .001)
    ;; (setf (linear obj) 0.35)
    ;; (setf (quadratic obj) 0.44)
    ))



