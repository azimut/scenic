(in-package #:scenic)

(defclass monster (assimp-thing-with-bones textured animated)
  ())

(defun make-monster ()
  (mapcar
   (lambda (obj)
     (destructuring-bind (&key buf albedo normals specular bones scene duration &allow-other-keys) obj
       (make-instance 'monster
                      :current :walking
                      :animations '((:idle)
                                    (:walking :start 24.8 :end 28.5 :inc 0.02 :loop-p T :index 0))
                      :buf buf
                      :albedo albedo
                      :normal normals
                      :specular specular
                      :bones bones
                      :duration duration
                      :rot (q:from-axis-angle (v! 1 0 0) (radians 90))
                      :scale .2
                      :scene scene)))
   (assimp-load-meshes "static/monster/forestmonster.b3d")))

(defun make-monster ()
  (mapcar
   (lambda (obj)
     (destructuring-bind (&key buf albedo normals specular bones scene duration &allow-other-keys) obj
       (make-instance 'monster
                      :buf buf
                      :albedo albedo
                      :normal normals
                      :specular specular
                      :bones bones
                      :duration duration
                      :rot (q:from-axis-angle (v! 1 0 0) (radians 90))
                      :scale .4
                      :scene scene)))
   (assimp-load-meshes "static/guard/boblampclean.md5mesh")))

(defpipeline-g textured-bone-pipe ()
  (vert-with-tbdata-defer-bones g-pnt tb-data assimp-bones)
  (textured-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (actor monster) (camera defered) time)
  (with-slots (buf scale albedo normal aomap color bones) actor
    (map-g #'textured-bone-pipe buf
           :color color
           :aomap aomap
           :albedo albedo
           :offsets bones
           :normal-map normal
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale)))

(defmethod update ((actor monster) _)
  #+nil
  (with-slots (scene bones clock index) actor
    (when index
      (push-g (get-bones-time-tranforms scene index clock) bones))))


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

(let ((e (first (lights (current-scene)))))
  (defmethod update ((obj point) dt)
    #+nil
    (when (eq e obj)
      (god-move 5000 dt obj))))

;; (defun cloud:get-listener-pos () (pos (current-camera)))
;; (defun cloud-get-listener-rot () (rot (current-camera)))

(defmethod update ((camera perspsective) dt)
  (god-move .1 dt camera)
  (full-rot .1 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defmethod update ((camera defered) dt)
  (god-move .1 dt camera)
  (full-rot .1 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

(defun match-camera ()
  "makes the camera of the next scene match the current one"
  (let* ((next-idx (mod (1+ (scene-index *state*)) (length (scenes *state*))))
         (other-cam (active-camera (nth next-idx (scenes *state*)))))
    (setf (pos other-cam) (copy-seq (pos (current-camera))))
    (setf (rot other-cam) (copy-seq (rot (current-camera))))))

(defmethod update ((camera perspective) dt)
  ;; (when (key-down-p key.j) (rocketman::next-row *rocket*))
  ;; (when (key-down-p key.k) (rocketman::prev-row *rocket*))
  ;; (when (key-down-p key.space) (rocket-pause-toggle))
  ;; (setf (fov camera) (rocket-get "camera:fov"))
  ;; (setf (pos camera) (rocket-get-v3 "camera:x" "camera:y" "camera:z"))
  ;; (let ((new (q:point-at (v! 0 1 0) (pos camera)
  ;;                        (rocket-get-v3 "view:x" "view:y" "view:z"))))
  ;;   (unless (q:= (rot camera) new)
  ;;     (setf (rot camera) new)))
  ;; ;;
  ;; (let ((light (first (lights (current-scene))))
  ;;       (new-value (rocket-get-v3 "point1:x" "point1:y" "point1:z")))
  ;;   (unless (v3:= new-value (pos light))
  ;;     (setf (pos light) new-value)))
  (god-move 2000 dt camera)
  (full-rot 2000 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )


(defun init-all-the-things ()
  (init-state (list (make-material :roughness .8 :metallic .02 :specular .1)
                    (make-material :roughness .8 :metallic .02 :specular .1)))
  (let ((s1 (make-scene)))
    (register (make-perspective :scale .25 :pos (v! 2 2 2) :rot (q:point-at (v! 0 1 0) (v! 2 2 2) (v! 0 0 0))) s1)
    (register (make-point :pos (v! -2 2 -2) :color (v! .1 .1 .3) :linear 0.35 :quadratic 0.44) s1)
    (register (make-box) s1)
    (register s1 *state*)))

(defmethod update ((obj directional) dt)
  (let* ((new-pos (v3:*s (v! 20 10 -30) 1f0))
         (new-dis (v3:distance new-pos (v! 0 0 0))))
    ;; (setf (pos obj) new-pos)
    ;; (setf (rot obj)  (q:point-at (v! 0 1 0) new-pos (v! 0 0 0)))
    ;; (setf (far obj)  (+ new-dis (* new-dis .1)))
    ;; (setf (near obj) (- new-dis (* new-dis .1)))
    ;; (setf (fs obj) (v2! 10))
    ))

