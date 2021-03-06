(in-package :incandescent)

(defclass die (untextured physic-box)
  ())

(defun make-die (&rest rest &key (scene (current-scene)) (x 1f0) (y 1f0) (z 1f0) &allow-other-keys)
  (remf rest :x)
  (remf rest :y)
  (remf rest :z)
  (remf rest :scene)
  (let ((buf (box x y z)))
    (apply #'make-instance 'die :buf buf :scene scene rest)))

(defun init-all-the-things ()
  (init-state (list (make-material :roughness .8 :metallic .02 :specular .1)))
  (let ((s1 (make-scene)))
    (register (make-perspective :scale .25 :pos (v! 2 3 2) :rot (q:point-at (v! 0 1 0) (v! 2 3 2) (v! 0 0 0))) s1)
    (register (point-size (make-point :pos (v! -2 3 -2) :color (v! .1 .1 .3)) 3) s1)
    (register (point-size (make-point :pos (v! 2 3 2) :color (v! .3 .1 .2)) 3) s1)
    (register (make-die :pos (v! 0 2 0)) s1)
    (register (make-box 10f0  1f0 10f0 :pos (v! 0 -0.5 0)) s1)
    (register s1 *state*)))


#+nil
(progn (free-actors)
       (make-piso)
       (make-physic-newcyl :radius .9f0
                           :height 3f0
                           :pos (v! -3 1 -3)
                           :density 3f0
                           :rot (q:from-axis-angle (v! 0 0 1) (radians 89))
                           )
       (make-physic-cone :pos (v! 0 1 0))
       (make-physic-cone :pos (v! 3 1 3)))

(progn (defun test ()
         ;;(free-actors)
         ;; (ode-destroy)
         ;; (ode-init)
         ;;(make-piso)
         (dotimes (i 30)
           (make-physic-box
            ;;:immovablep t
            :rot (q:from-axis-angle (v! (random 1f0)
                                        (random 1f0)
                                        (random 1f0))
                                    (radians (random 360f0)))
            :pos (v! (serapeum:random-in-range -5 5)
                     (serapeum:random-in-range 10   30)
                     (serapeum:random-in-range -15 -5)))))
       (test))

(progn (defun test ()
         (free-actors)
         (make-piso)
         (loop :for i :from 1 :by 2 :to 20
               :for y :from 1 :by .7 :to 10
               :do (make-physic-box
                    :rot (q:from-axis-angle (v! 0 0 1) (radians (random 100)))
                    :pos (v! i (+ y .5) 10))))

       (test)
       )

(progn
  (defun test ()
    (free-actors)
    (make-piso)
    (loop :for x :from 1 :by 1 :to 9
          :do (loop :for y :from 0 :to 3
                    :do (make-physic-box :pos (v! x (+ y .5) 0))))
    (loop :for z :from 0 :by 1 :to 9
          :do (loop :for y :from 0 :to 3
                    :do (make-physic-box :pos (v! 0 (+ y .5) z))))
    (loop :for y :from 0 :by 1 :to 10
          :do (if (< (random 1f0) .5)
                  (make-physic-box :pos (v! (+ 1 (random 1)) (+ y .5) 0))
                  (make-physic-sphere :pos (v! (+ 1 (random 1)) (+ y .5) 0))))
    ;; (loop :for z :from 0 :by 1 :to 10
    ;;       :do (make-physic-box :pos (v! 11 .5 z)))
    ;; (loop :for x :from 1 :by 1 :to 10
    ;;       :do (make-physic-box :pos (v! x .5 10)))
    )

  (test)
  )



(progn (defun test ()
         (free-actors)
         (make-piso)
         (loop :for i :from 1 :by 1.9 :to 11
               :do (make-physic-box :pos (v! i .5 10)))
         (loop :for i :from 2 :by 1.87 :to 10
               :do (make-physic-box :pos (v! i 1.5 10)))
         ;;--------------------------------------------------
         (loop :for i :from 1 :by 1.9 :to 11
               :do (make-physic-box :pos (v! i .5 5)))
         (loop :for i :from 2 :by 1.87 :to 10
               :do (make-physic-box :pos (v! i 1.5 5)))
         )
       (test)
       )

(progn (defun test ()
         (free-actors)
         (make-piso :pos (v! 0 -2 0))
         (loop :for y :from 0 :by 1 :to 10
               :do (make-physic-box :pos (v! 0 (+ y .5) 1)))
         (loop :for y :from 0 :by 1 :to 10
               :do (make-physic-box :pos (v! 0 (+ y .5) 0)))
         (loop :for y :from 0 :by 1 :to 10
               :do (make-physic-box :pos (v! 1 (+ y .5) 0)))
         (make-physic-sphere :pos (v! 0 0 0))
         ;;--------------------------------------------------
         )

       (test)
       )

(progn (defun test ()
         (free-actors)
         (make-piso)
         (make-physic-box :pos (v! 0 5.5 0) :x 8f0 :z 8f0)
         (make-physic-cone :pos (v! 0 2 0))
         (loop :for y :from .5 :by 1 :to 5
               :do (make-physic-box :pos (v! 3 y 3)))
         (loop :for y :from .5 :by 1 :to 5
               :do (make-physic-box :pos (v! -3 y -3)))
         (loop :for x :from -2 :to 3
               :for z :from -2 :to 3
               :do (make-physic-sphere :pos (v! x 6.5 z)))
         ;;--------------------------------------------------
         )

       (test)
       )


;;--------------------------------------------------

(progn
  (free-actors)
  (make-piso)
  (make-physic-box
   :immovablep t
   :pos (v! -4 10 0)
   :y 10f0
   :z 20f0)
  (dotimes (i 10)
    (make-physic-sphere
     :radius .75f0
     :pos (v! (serapeum:random-in-range 0f0 -2f0)
              (serapeum:random-in-range 15f0 30f0)
              (serapeum:random-in-range -10f0 0f0))))
  (loop :for y-offset :by 2 :to 10
        :for wide-offset :in (serapeum:repeat-sequence '(1.25 0) 10)
        :do (loop :for x from 0 to 5
                  :for wide :from -10 :by 2.5 :to 10
                  :do (make-physic-newcyl
                       :immovablep t
                       :radius .25f0
                       :height 3f0
                       :pos (v! 0 (+ 4 y-offset) (+ wide-offset wide))
                       :rot (q:from-axis-angle (v! 0 0 1)
                                               (radians 90)))))
  #+nil
  (loop :for i :from -8.5 :by 2.5  :to 10
        :do (make-physic-newcyl
             :immovablep t
             :radius .25f0
             :height 3f0
             :pos (v! 0 6 i)
             :rot (q:from-axis-angle (v! 0 0 1) (radians 90)))))


;;--------------------------------------------------
(progn (defun test ()
         ;;(free-actors)
         ;; (ode-destroy)
         ;; (ode-init)
         ;;(make-piso)
         (dotimes (i 50)
           (make-physic-sphere
            
            ;;:immovablep t
            :rot (q:from-axis-angle (v! (random 1f0)
                                        (random 1f0)
                                        (random 1f0))
                                    (radians (random 360f0)))
            :pos (v! (serapeum:random-in-range -5 5)
                     0
                     (serapeum:random-in-range -13 -7)))))
       (test))
