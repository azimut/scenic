(in-package #:scenic)

(defclass building-physics (derived untextured)
  ((scene :initarg :scene)
   (name  :initarg :name))
  (:default-initargs
   :shadowp T
   :drawp T
   :immovablep T))

(defun make-building-physics (space)
  (mapcar (lambda (plist)
            (destructuring-bind (&key name buf scene &allow-other-keys) plist
              (make-instance 'building-physics
                             :space space
                             :rot (q:*(q:from-axis-angle (v! 0 0 1) (radians 90))
                                      (q:from-axis-angle (v! 0 1 0) (radians 90)))
                             :buf buf
                             :name name
                             :scene scene
                             :scale 1f0)))
          (assimp-load-meshes "/home/sendai/hotel.fbx_Physics_001/hotel.fbx_Physics_001.fbx")))

(defclass building (assimp-thing textured)
  ())

(defun make-building ()
  (mapcar
   (lambda (obj)
     (destructuring-bind (&key name buf albedo normals specular scene roughmap &allow-other-keys)
         obj
       (declare (ignore specular))
       (make-instance 'building
                      :name name
                      :rot (q:*(q:from-axis-angle (v! 0 0 1) (radians 90))
                               (q:from-axis-angle (v! 0 1 0) (radians 90)))
                      :buf buf
                      :albedo albedo
                      :normal normals
                      :roughmap roughmap
                      :scale 1.2
                      :scene scene)))
   (assimp-load-meshes "/home/sendai/hotel.fbx_Meshuga/hotel.fbx_Meshuga.fbx")
   ;;(assimp-load-meshes "/home/sendai/hotel.fbx_Scene_Master_Collection/hotel.fbx_Scene_Master_Collection.fbx")
   ;;(assimp-load-meshes "/home/sendai/hotel.fbx")
   ;;(assimp-load-meshes "/home/sendai/hotel.fbx_Walls_002/hotel.fbx_Walls_002.fbx")
   ;;(assimp-load-meshes "/home/sendai/hotel.fbx_Walls_001/hotel.fbx_Walls_001.fbx")
   ;;(assimp-load-meshes "/home/sendai/hotel.fbx_Scene/hotel.fbx_Scene.fbx")
   ))

;;----------------------------------------

(defun test-load ()
  (free-assimp-buffers)
  (let ((buf (getf (car (assimp-load-meshes "/home/sendai/hotel.fbx_Walls_002/hotel.fbx_Walls_002.fbx")) :buf)))
    (setf *tmp1* buf)
    (setf *tmp1-index* (pull-g (second (buffer-stream-gpu-arrays buf))))
    (setf *tmp1-array* (pull-g (car (first (buffer-stream-gpu-arrays buf)))))
    (assimp-load-meshes "/home/sendai/hotel.fbx_Walls_001/hotel.fbx_Walls_001.fbx")
    (setf *tmp1-update-index* (pull-g (second (buffer-stream-gpu-arrays buf))))
    (setf *tmp1-update-array* (pull-g (car (first (buffer-stream-gpu-arrays buf)))))))




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
                      :scale .1
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



(defmethod handle ((e tick) (actor monster))
  (with-slots (scene bones clock index) actor
    (when index
      (when-let ((inc (inc actor)))
        (incf (clock actor) (+ 0.02 inc)))
      (mapcar (lambda (l) (setf (drawp l) T)) (lights (current-scene)))
      (push-g (get-bones-time-tranforms scene index clock) bones))))

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

(defmethod update ((obj defered) dt)
  (%ode:body-enable (body obj))
  (human-move .1 dt obj)
  (full-rot .1f0 dt obj)
  (setf (slot-value obj 'pos)
        (v3:+ (ode-geom-get-position (slot-value obj 'geom))
              (v! 0 .4 0))))

(defmethod update ((camera perspective) dt)
  (god-move .1 dt camera)
  (full-rot .1 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

;; https://github.com/rafaeldelboni/raylib-pixelated-fps/blob/c9b04f9013f74a4fde9b6df244581e7b0d2df3e0/app/ode_app.c
(let ((mouse-pos-previous (v! 0 0))
      (mouse-pos-delta    (v! 0 0))
      (mouse-pos          (v! 0 0))
      (angle              (v! 0 0))
      (vel                (v! 0 0 0)))
  (defmethod update ((obj defered) dt)
    (setf angle (v2:*s mouse-pos-delta -0.003))
    (let* ((xxx (- (* +PI+ 2) (x angle)))
           (yyy (- (* +PI+ 2) (y angle)))
           (aimv (v! (* (sin xxx) (cos yyy))
                     (sin yyy)
                     (* (cos xxx) (cos yyy)))))
      (setf (rot obj) (q:look-at (v! 0 1 0)
                                 (pos obj)
                                 aimv)))
    (m3:rotation-from-euler (- (* 2 +pi+) (y angle))
                            (- (* 2 +pi+) (x angle))
                            0)
    (setf vel (v3:*
               (v! 10 0 10)
               (v3:normalize
                (v! (/ (+ (- (if (key-down-p key.d) (sin (x angle)) 0f0)
                             (if (key-down-p key.a) (sin (x angle)) 0f0)
                             (if (key-down-p key.w) (cos (x angle)) 0f0))
                          (if (key-down-p key.s) (cos (x angle)) 0f0))
                       20f0)
                    (/ (- (if (key-down-p key.a) (sin (y angle)) 0f0)
                          (if (key-down-p key.d) (sin (y angle)) 0f0))
                       20f0)
                    (/ (- (+ (- (if (key-down-p key.d) (cos (x angle)) 0f0)
                                (if (key-down-p key.a) (cos (x angle)) 0f0))
                             (if (key-down-p key.w) (sin (x angle)) 0f0))
                          (if (key-down-p key.s) (sin (x angle)) 0f0))
                       20f0)))))
    (cond ((> (y angle) (radians  89)) (setf (y angle) (radians  89)))
          ((< (y angle) (radians -89)) (setf (y angle) (radians -89))))
    (%ode:body-set-angular-vel (body obj) (x vel) (y vel) (z vel))
    (setf mouse-pos         (mouse-pos (mouse)))
    (setf mouse-pos-delta   (v2:- mouse-pos mouse-pos-previous))
    (setf mouse-pos-previous mouse-pos)

    ;; aim_vector
    (%ode:body-enable (body obj))
    ;;(full-rot .1f0 dt obj)
    (setf (slot-value obj 'pos) (ode-geom-get-position (slot-value obj 'geom)))
    ;; (let ((pos (v! 2 4 4)))
    ;;   (setf (pos camera) dpos)
    ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
    ))

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
  (god-move 10f0 dt camera)
  (full-rot 10f0 dt camera)
  ;; (let ((pos (v! 2 4 4)))
  ;;   (setf (pos camera) pos)
  ;;   (setf (rot camera) (q:point-at (v! 0 1 0) pos (v! 0 0 0))))
  )

;;------------------------------
;; AOC 18


(defun init-all-the-things ()
  (init-state
   (list (make-material :roughness .8 :metallic .02 :specular .1)
         (make-material :roughness .4 :metallic .4  :specular .1)))
  (let ((s1 (make-scene-ode :color (v! .2 .2 .2 1) :post (list (make-defer-postprocess)))))
    #+nil
    (register (make-defered :downscale .25 :pos (v! -14.65844 4.4909353 0.22071815)
                            :rot (q! 0.95456934 -0.0030270235 -0.297679 0.013245501))
              s1)
    (mapcar (lambda (a) (register a s1)) (make-building-physics (space s1)))
    (register (make-physic-camera :pos (v! -9.748983 6.841267 -0.19243619)
                                  :space (space s1)
                                  :downscale .25)
              s1)
    ;;(register (make-directional :pos (v! 150 333 223)) s1)
    (register (make-point :pos (v! -10 5  0) :far 20f0 :fudge 0.2)  s1)
    (register (make-point :pos (v! -11 8 -5) :far 20f0 :fudge 0.08) s1)
    (register s1 *state*)))
