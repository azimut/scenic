(in-package #:scenic)

(defclass foo (physic-box textured)
  ())

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

(defmethod update ((camera physic-camera) dt)
  ;; (tank-move-ode camera .9)
  ;; (tank-rot .2 dt camera)
  (human-move-ode camera 1f0)
  (human-rot .5 dt camera)
  ;; (god-move .9 dt camera)
  ;; (full-rot .5 dt camera)
  )

;; (defvar *sfx*
;;   (nepal:make-sfx :newpitter '("/home/sendai/bc.wav")
;;                   :pos (copy-seq (pos (current-camera)))
;;                   :loop-p t))
;;(nepal::play *sfx*)
;; (nepal::stop *sfx*)

(defmethod update ((obj directional) dt)
  #+nil
  (let* ((new-pos (v3:*s (v! -20 10 -30) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    (setf (pos obj) new-pos)
    (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    (setf (far obj)  (+ new-dis (* new-dis .4)))
    (setf (near obj) (- new-dis (* new-dis .4)))
    (setf (fs obj) (v2! 20))
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

(defun init-all-the-things ()
  (init-state
   `(,(make-material :roughness .8 :metallic .02 :specular .1 :fakeambient .001)
     ,(make-material :roughness .4 :metallic .4  :specular .1)))
  ;;(defered-spot-bunny-floor-textured)
  ;;(forward-spot-bunny-floor-textured)
  (ode-human)
  )

;;----------------------------------------
;; Scenes

(defclass fps (camera-audio-ode)
  ((footsteps :reader footsteps)))

(defmethod (setf pos) :around (new-value (obj fps))
  (when
      (and (hit-floor-p obj)
           ;;(not (v3:= new-value (slot-value obj 'pos)))
           (or (key-down-p key.w)
               (key-down-p key.a)
               (key-down-p key.s)
               (key-down-p key.d)))
    (nepal::play (footsteps obj)))
  (call-next-method))

(defmethod (setf hit-floor-p) :before (new-value (obj fps))
  (alexandria:when-let ((landing-p (and new-value (not (hit-floor-p obj)))))
    (nepal::stop (footsteps obj))
    (nepal::play (footsteps obj))))

(defmethod initialize-instance :after ((obj fps) &key)
  (setf (slot-value obj 'footsteps)
        (make-instance
         'nepal:event
         :name :steps
         :pos (v! 0 -2 0)
         :step-size 0.25
         :overlap-p T
         :gain .5
         :paths (resolve-paths
                 "static/SFX/Step/Step1.wav"
                 "static/SFX/Step/Step2.wav"
                 "static/SFX/Step/Step3.wav"
                 "static/SFX/Step/Step4.wav"
                 "static/SFX/Step/Step5.wav"
                 "static/SFX/Step/Step6.wav"
                 "static/SFX/Step/Step7.wav"
                 "static/SFX/Step/Step8.wav"))))

(defun ode-human ()
  (let* ((s1  (make-scene-ode-ibl
               :name "defered ibl audio ode"
               :post (list (make-instance 'hdr-acesfilm :exposure .8)
                           (make-instance 'fxaa3)))))
    (register s1 *state*) ;; first registered, needed for ODE scenes
    ;;(register (make-physic-camera :pos (v! 2 1 2) :fakeambient .01) s1)
    (register (make-instance 'fps :pos (v! 2 1 2)) s1)
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
    (register (make-sky) s1)
    (register (make-untextured-floor) s1)
    ;;(register (make-textured-floor) s1)
    (register (make-some-point) s1)
    ;;(register (make-some-directional) s1)
    (register (make-instance
               'physic-box
               :pos (v! 0 4 0)
               :rot (q:from-axis-angle (v! 0 0 1) (radians (random 360f0))))
              s1)
    (register (make-instance
               'physic-box
               :pos (v! 3 10 3)
               :rot (q:from-axis-angle (v! 1 0 0) (radians (random 360f0))))
              s1)))

(defun defered-spot-bunny-floor-textured ()
  (let ((s1 (make-scene-ibl
             :name "defered bunny spotlight")))
    (register s1 *state*)
    (register (make-defered
               :pos (v! -2 2 -2)
               :rot (q:point-at (v! 0 1 0) (v! -2 2 -2) (v! 0 0 0)))
              s1)
    (register (make-sky) s1) ;; !!!!
    ;;(register (make-clouds) s1)
    ;;(register (make-some-spot) s1)
    (register (make-some-directional) s1)
    (register-untextured-random-columns s1)
    ;;(register (make-bunny) s1)
    ;;(register (make-untextured-floor) s1)
    (register (make-textured-floor) s1)))

(defun forward-spot-bunny-floor-textured ()
  (let ((s1 (make-scene-ibl
             :name "forward-ibl bunny spotlight")))
    (register s1 *state*)
    (register (make-perspective
               :pos (v! -2 2 -2)
               :rot (q:point-at (v! 0 1 0) (v! -2 2 -2) (v! 0 0 0)))
              s1)
    (register (make-sky) s1);; !!!!
    ;;(register (make-some-directional) s1)
    (register (make-some-spot) s1)
    (register-untextured-random-columns s1)
    (register (make-bunny) s1)
    (register (make-textured-floor) s1)
    ;;(register (make-untextured-floor) s1)
    ))

;;----------------------------------------
;; Things

(defun register-untextured-random-columns (s1)
  (dotimes (i 20)
    (register (make-instance 'untextured
                             :buf (box 1f0 5f0 1f0)
                             :pos (v! (random-in-range -15f0 15f0)
                                      (random-in-range 1.0 2.5)
                                      (random-in-range -15f0 15f0)))
              s1)))

(defun make-some-point ()
  (make-point
   :pos (v! -2 2 -2)
   :color (light-color 11)
   :linear 0.35
   :quadratic 0.44))

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
   'textured-pbr
   ;;:material 1 ; for fake ambient
   :uv-repeat (v2! 4)
   :buf (lattice 100f0 100f0 500 500 t)))

;; bunny.obj            - has no UVs/TEXTURE-COORDS
;; boblampclean.md5mesh - has no NORMALS by default, has only albedo textures
(defun make-bunny ()
  (make-instance
   'untextured
   :buf (assimp-load-mesh "static/bunny.obj")
   :pos (v! 0 0.85 0)))

(defun make-sky () (make-instance 'sky :intensity 15f0))
(defun make-clouds ()
  (make-envmap "static/null/ThickCloudsWater/left.png"
               "static/null/ThickCloudsWater/right.png"
               "static/null/ThickCloudsWater/up.png"
               "static/null/ThickCloudsWater/down.png"
               "static/null/ThickCloudsWater/front.png"
               "static/null/ThickCloudsWater/back.png"))
