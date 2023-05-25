(in-package #:scenic)

(defmethod update ((camera perspective) dt)
  (god-move .2 dt camera)
  (full-rot .2 dt camera))

(defmethod update ((camera defered) dt)
  (god-move .2 dt camera)
  (full-rot .2 dt camera))

(defun init-all-the-things ()
  (init-state
   `(,(make-material :roughness .8 :metallic .02 :specular .1)
     ,(make-material :roughness .4 :metallic .4  :specular .1)))

  (let ((s1  (make-scene-ibl))
        (cam (make-perspective
              :pos (v! 2 2 2)
              :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
        (lp1 (point-size
              (make-point
               :pos (v! -1 2 -1)
               :color (v! .1 .1 .3))
              4)))
    (register (make-box) s1)
    (register cam s1)
    (register lp1 s1)
    (register s1 *state*))

  (let ((s1 (make-scene))
        (cam (make-perspective
              :pos (v! 2 2 2)
              :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))))
        (lp1 (make-point
              :pos (v! -2 2 -2)
              :color (v! .1 .1 .4)
              :linear 0.35
              :quadratic 0.44)))
    ;; (register (make-cube "static/ThickCloudsWater/left.png"
    ;;                      "static/ThickCloudsWater/right.png"
    ;;                      "static/ThickCloudsWater/up.png"
    ;;                      "static/ThickCloudsWater/down.png"
    ;;                      "static/ThickCloudsWater/front.png"
    ;;                      "static/ThickCloudsWater/back.png")
    ;;           s1)
    (register cam s1)
    (register lp1 s1)
    (register (make-directional :color (light-color 0) :pos (v! 150 333 223)) s1)
    (register (make-box) s1)
    (register s1 *state*))

  (let ((s1 (make-scene
             :color (v! .2 .2 .2 1)
             :post (list (make-defer-postprocess))))
        (ls1 (make-spot
              :far 20f0 :near 4f0
              :pos (v! -2 8 -2)
              :color (v! .5 .1 .3)
              :rot (q:point-at (v! 0 1 0) (v! -2 8 -2) (v! 0 0 0))))
        (cam (make-defered
              :pos (v! 2 2 2)
              :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0)))))
    (register cam s1)
    (register ls1 s1)
    (register (make-box :w 20f0 :d 20f0 :pos (v! 0 -.5 0)) s1)
    (register s1 *state*)))
