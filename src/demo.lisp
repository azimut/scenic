(in-package #:scenic)

(defmethod hit-object ((obj physic-box))
  (print (list "hit physic-box" (pos obj))))

(defmethod update ((camera perspective) dt)
  (god-move .9 dt camera)
  (full-rot .5 dt camera))

(defmethod update ((camera defered) dt)
  ;; (tank-move-ode camera .9)
  ;; (tank-rot .2 dt camera)
  ;; (human-move-ode camera 1f0)
  ;; (human-rot .5 dt camera)
  (god-move .9 dt camera)
  (full-rot .5 dt camera)
  )

;; (defvar *sfx*
;;   (nepal:make-sfx :newpitter '("/home/sendai/bc.wav")
;;                   :pos (copy-seq (pos (current-camera)))
;;                   :loop-p t))
;;(nepal::play *sfx*)
;; (nepal::stop *sfx*)

(defmethod update ((obj directional) dt)
  #+nil
  (let* ((new-pos (v3:*s (v! -10 40 30) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    (setf (pos obj) new-pos)
    (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    (setf (far obj)  (+ new-dis (* new-dis .3)))
    (setf (near obj) (- new-dis (* new-dis .3)))
    (setf (fs obj) (v2! 40))
    ))

(defmethod update ((obj spot) dt)
  #+nil
  (let* ((new-pos (v3:*s (v! -2 8 -2) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    ;; (setf (pos obj) new-pos)
    ;; (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    (setf (near obj) (- new-dis (* new-dis .7)))
    (setf (far obj)  (+ new-dis (* new-dis 2)))
    (setf (fov obj) 100f0)
    ))
#+nil
(defun init-all-the-things ()
  (init-state
   `(,(make-material :roughness .8 :metallic .02 :specular .1)
     ,(make-material :roughness .4 :metallic .4  :specular .1)))
  #+nil
  (let* ((s1  (make-scene-ode :name "forward ode")))
    (register s1 *state*) ;; 1st needed for ODE scenes
    #+nil
    (register (make-physic-camera
               :pos (v! 2 1 2))
              s1)
    ;;#+nil
    (register (make-instance
               'camera-audio-ode
               :pos (v! 2 .3 2))
              s1)
    #+nil
    (register (make-instance
               'audio-sfx
               :name :weird
               :pos (v! 3 8 2)
               :loop-p t
               :max-distance 12f0
               :ref-distance 1.5f0
               :paths '("/home/sendai/bc.wav"))
              s1)
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
              s1))
  ;;#+nil
  (let ((s1  (make-scene-ibl)))
    (register s1 *state*)
    (register (make-perspective
               :pos (v! 2 2 2)
               :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0)))
              s1)
    (register (make-clouds) s1)
    (register (make-box :pos (v! 0 1 0)) s1)
    (register (make-untextured-floor) s1)
    (register (make-directional
               :pos (v! -20 40 50)
               :rot (q:point-at (v! 0 1 0) (v! -20 40 50) (v! 0 0 0))
               :near 60f0
               :far 80f0
               :fs (v2! 30)
               :color (light-color 5))
              s1)
    #+nil
    (register (point-size
               (make-point
                :pos (v! -1 2 -1)
                :color (v! .1 .1 .3))
               4)
              s1))
  #+nil
  (let ((s1 (make-scene-ibl
             :color (v! .2 .2 .4 1)
             :name "ibl forward"))
        (cam (make-perspective
              :pos (v! -2 4 -2)
              :rot (q:point-at (v! 0 1 0) (v! -2 4 -2) (v! 0 0 0))))
        )
    (register (make-clouds) s1)
    (register cam s1)
    ;; (register (make-spot
    ;;            :far 20f0 :near 4f0
    ;;            :pos (v! -2 8 -2)
    ;;            :color (light-color 10)
    ;;            :rot (q:point-at (v! 0 1 0) (v! -2 8 -2) (v! 0 0 0)))
    ;;           s1)
    ;; (register (make-point
    ;;            :pos (v! -3 3 -2)
    ;;            :color (light-color 11)
    ;;            :linear 0.35
    ;;            :quadratic 0.44)
    ;;           s1)
    ;; (register (make-billboards
    ;;            :sam (get-tex "static/monster/860_leaf.png"))
    ;;           s1)
    (register (make-directional
               :pos (v! -20 40 50)
               :rot (q:point-at (v! 0 1 0) (v! -20 40 50) (v! 0 0 0))
               :near 60f0
               :far 80f0
               :fs (v2! 30)
               :color (light-color 5))
              s1)
    (dotimes (i 20)
      (register (make-instance 'untextured
                               :buf (box 1f0 5f0 1f0)
                               :pos (v! (random-in-range -15f0 15f0)
                                        (random-in-range 1.0 2.5)
                                        (random-in-range -15f0 15f0)))
                s1))
    ;;(register (make-sky) s1)
    (register (make-bunny) s1)
    (register (make-untextured-floor) s1)
    (register s1 *state*))
  ;;#+nil
  (let ((s1 (make-scene-ibl
             :name "defered ibl"
             :color (v! .2 .2 .2 1)
             :post (list ;;(make-instance 'ssao)
                    (make-instance 'hdr-acesfilm)
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
    (register (make-clouds) s1)
    (register (make-directional
               :fudge 0.01
               :pos (v! -20 40 50)
               :rot (q:point-at (v! 0 1 0) (v! -20 40 50) (v! 0 0 0))
               :near 60f0
               :far 80f0
               :fs (v2! 40)
               :color (light-color 5))
              s1)
    (dotimes (i 20)
      (register (make-instance 'untextured
                               :buf (box 1f0 5f0 1f0)
                               :pos (v! (random-in-range -15f0 15f0)
                                        (random-in-range 1.0 2.5)
                                        (random-in-range -15f0 15f0)))
                s1))
    (register (make-box :h 10 :w 2f0 :d 2f0 :pos (v! 8 5   9)) s1)
    (register (make-box :h 10 :w 2f0 :d 2f0 :pos (v! 8 5 -11)) s1)
    (register (make-bunny) s1)
    (register (make-untextured-floor) s1)
    (register s1 *state*)))

(defun init-all-the-things ()
  (init-state
   `(,(make-material :roughness .8 :metallic .02 :specular .1 :fakeambient .001)
     ,(make-material :roughness .4 :metallic .4  :specular .1)))
  (defered-spot-bunny-floor-textured)
  ;;(forward-spot-bunny-floor-textured)
  )

(defun defered-spot-bunny-floor-textured ()
  (let ((s1 (make-scene-ibl
             :name "ibl defered bunny spotlight")))
    (register s1 *state*)
    (register (make-defered
               :pos (v! -2 2 -2)
               :rot (q:point-at (v! 0 1 0) (v! -2 2 -2) (v! 0 0 0)))
              s1)
    (register (make-sky) s1);; !!!!
    (register (make-some-spot) s1)
    ;;(register (make-some-directional) s1)
    (dotimes (i 20)
      (register (make-instance 'untextured
                               :buf (box 1f0 5f0 1f0)
                               :pos (v! (random-in-range -15f0 15f0)
                                        (random-in-range 1.0 2.5)
                                        (random-in-range -15f0 15f0)))
                s1))
    (register (make-bunny) s1)
    ;;(register (make-textured-floor) s1)
    (register (make-untextured-floor) s1)
    ))

(defun forward-spot-bunny-floor-textured ()
  (let ((s1 (make-scene
             :name "forward bunny spotlight")))
    (register s1 *state*)
    (register (make-perspective
               :pos (v! -2 2 -2)
               :rot (q:point-at (v! 0 1 0) (v! -2 2 -2) (v! 0 0 0)))
              s1)
    (register (make-sky) s1);; !!!!
    ;;(register (make-some-directional) s1)
    (register (make-some-spot) s1)
    (dotimes (i 20)
      (register (make-instance 'untextured
                               :buf (box 1f0 5f0 1f0)
                               :pos (v! (random-in-range -15f0 15f0)
                                        (random-in-range 1.0 2.5)
                                        (random-in-range -15f0 15f0)))
                s1))
    (register (make-bunny) s1)
    ;;(register (make-textured-floor) s1)
    (register (make-untextured-floor) s1)
    ))

(defun make-some-spot ()
  (make-spot
   :cutoff 0.2 :outer-cutoff 0.8
   :fov 100f0
   :far 16.12 :near 5.939
   :pos (v! -2 8 -2)
   :color (light-color 10)
   :rot (q:point-at (v! 0 1 0) (v! -2 8 -2) (v! 0 0 0))))

(defun make-some-directional ()
  (make-directional
   :pos (v! -20 40 50)
   :rot (q:point-at (v! 0 1 0) (v! -20 40 50) (v! 0 0 0))
   :near 60f0
   :far 80f0
   :fs (v2! 30)
   :color (light-color 5)))

(defun make-untextured-floor ()
  (make-instance
   'untextured
   :buf (lattice 100f0 100f0)))

(defun make-textured-floor ()
  (make-instance
   'textured
   :material 1
   :buf (lattice 100f0 100f0 500 500 t)))

(defun make-bunny ()
  (make-instance
   'untextured
   :buf (assimp-load-mesh "static/bunny.obj")
   :pos (v! 0 0.85 0)))

(defun make-sky () (make-instance 'sky :intensity 15f0))
(defun make-clouds ()
  (make-envmap "static/ThickCloudsWater/left.png"
               "static/ThickCloudsWater/right.png"
               "static/ThickCloudsWater/up.png"
               "static/ThickCloudsWater/down.png"
               "static/ThickCloudsWater/front.png"
               "static/ThickCloudsWater/back.png"))

(in-package :%alut)
(define-foreign-library alut
  (:windows "alut.dll" :calling-convention :stdcall)
  (:darwin (:or (:default "libalut") (:framework "alut")))
  (:unix (:or "libalut.so" "libalut.so.0" "libalut.so.0.1.0"))
  (t (:default "libalut")))
