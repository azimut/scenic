(in-package #:scenic)

(defmethod update ((camera perspective) dt)
  (god-move .2 dt camera)
  (full-rot .2 dt camera))

(defmethod update ((camera defered) dt)
  (god-move .2 dt camera)
  (full-rot .2 dt camera))

(defmethod update ((obj directional) dt)
  #+nil
  (let* ((new-pos (v3:*s (v! -20 30 20) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    (setf (pos obj) new-pos)
    (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    (setf (far obj)  (+ new-dis (* new-dis .2)))
    (setf (near obj) (- new-dis (* new-dis .1)))
    (setf (fs obj) (v2! 30))
    ))

(defun init-all-the-things ()
  (init-state
   `(,(make-material :roughness .8 :metallic .02 :specular .1)
     ,(make-material :roughness .4 :metallic .4  :specular .1)))
  (let* ((s1  (make-scene-ode :name "forward ode"))
         (cam (make-perspective
               :pos (v! 2 2 2)
               :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0)))))
    (register s1 *state*)
    (register (make-box :w 20f0 :d 20f0 :pos (v! 0 -.5 0))
              s1)
    (register (make-point
               :pos (v! -2 2 -2)
               :color (light-color 11)
               :linear 0.35
               :quadratic 0.44)
              s1)
    (register (make-instance
               'physic-box
               :pos (v! 0 4 0)
               :rot (q:from-axis-angle (v! 0 1 0) (radians (random 360f0))))
              s1)
    (register (make-instance
               'physic-box
               :pos (v! 3 10 3)
               :rot (q:from-axis-angle (v! 0 1 0) (radians (random 360f0))))
              s1)
    (register cam s1))
  #+nil
  (let ((s1  (make-scene-ibl))
        (cam (make-perspective
              :pos (v! 2 2 2)
              :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
        (lp1 (point-size
              (make-point
               :pos (v! -1 2 -1)
               :color (v! .1 .1 .3))
              4)))
    (register (make-box :pos (v! 0 1 0)) s1)
    (register (make-box :w 20f0 :d 20f0 :pos (v! 0 -.5 0)) s1)
    (register cam s1)
    (register lp1 s1)
    (register s1 *state*))
  #+nil
  (let ((s1 (make-scene
             :name "forward"
             :post (list ;;(make-dither)
                    (make-simple-postprocess))))
        (cam (make-perspective
              :pos (v! -2 4 -2)
              :rot (q:point-at (v! 0 1 0) (v! -2 4 -2) (v! 0 0 0))))
        )
    ;; (register (make-cube "static/ThickCloudsWater/left.png"
    ;;                      "static/ThickCloudsWater/right.png"
    ;;                      "static/ThickCloudsWater/up.png"
    ;;                      "static/ThickCloudsWater/down.png"
    ;;                      "static/ThickCloudsWater/front.png"
    ;;                      "static/ThickCloudsWater/back.png")
    ;;           s1)
    (register cam s1)
    ;; (register (make-spot
    ;;            :far 20f0 :near 4f0
    ;;            :pos (v! -2 8 -2)
    ;;            :color (light-color 10)
    ;;            :rot (q:point-at (v! 0 1 0) (v! -2 8 -2) (v! 0 0 0)))
    ;;           s1)
    (register (make-point
               :pos (v! -2 2 -2)
               :color (light-color 11)
               :linear 0.35
               :quadratic 0.44)
              s1)
    (register (make-billboards
               :sam (get-tex "static/monster/860_leaf.png"))
              s1)
    (register (make-instance
               'untextured
               :buf (assimp-load-mesh "static/bunny.obj")
               :pos (v! 0 0.85 -3))
              s1)
    (register (make-box :w 20f0 :d 20f0 :pos (v! 0 -.5 0)) s1)
    (register s1 *state*))
  #+nil
  (let ((s1 (make-scene
             :name "defered"
             :color (v! .2 .2 .2 1)
             :post (list ;;(make-instance 'ssao)
                    (make-simple-postprocess)
                    ;;(make-instance 'dof)
                    )))
        (cam (make-defered
              :pos (v! -2 2 -2)
              :rot (q:point-at (v! 0 1 0) (v! -2 2 -2) (v! 0 0 0)))))
    (register cam s1)
    #+nil
    (register (make-spot
               :far 20f0 :near 4f0
               :pos (v! -2 8 -2)
               :color (light-color 10)
               :rot (q:point-at (v! 0 1 0) (v! -2 8 -2) (v! 0 0 0)))
              s1)
    (register (make-directional
               :pos (v! -20 40 50)
               :rot (q:point-at (v! 0 1 0) (v! -20 40 50) (v! 0 0 0))
               :near 60f0
               :far 80f0
               :fs (v2! 30)
               :color (light-color 5))
              s1)
    (register (make-box :h 10 :w 2f0 :d 2f0 :pos (v! 8 5   9)) s1)
    (register (make-box :h 10 :w 2f0 :d 2f0 :pos (v! 8 5 -11)) s1)
    (register (make-instance
               'untextured
               :buf (assimp-load-mesh "static/bunny.obj")
               :pos (v! 0 0.85 0))
              s1)
    (register (make-textured
               :pos (v! 0 -.5 0)
               :buf (box 20f0 1f0 20f0 t))
              s1)
    (register s1 *state*)))
