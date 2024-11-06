(uiop:define-package #:scenic
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre))
  (:use #:cepl
        #:cepl.skitter
        #:cl
        #:livesupport
        #:nineveh
        #:rtg-math
        #:vari
        #:with-setf)
  (:shadow #:space)
  (:export
   #:*state*
   #:start
   #:stop
   #:register
   #:update
   #:init-state
   #:init-all-the-things
   #:current-scene
   #:current-camera
   #:assimp-load-mesh
   #:make-material
   #:make-scene
   #:make-scene-ibl
   #:make-scene-ode-ibl
   #:make-scene-ode
   #:make-scene-vxgi
   #:textured-pbr
   #:untextured
   ;; camera
   #:fps
   #:make-defered
   #:make-perspective
   #:perspective
   #:defered
   #:make-physic-camera
   #:physic-camera
   ;; controls
   #:god-move
   #:human-move
   #:human-rot
   #:tank-rot
   #:full-rot
   ;; shapes
   #:box
   #:sphere
   #:lattice
   #:physic-box
   ;; lights
   #:make-directional
   #:make-point
   #:make-spot
   #:directional
   #:point
   #:spot
   #:light-color
   ;; accessors
   #:color
   #:pos
   #:rot
   )
  (:import-from #:temporal-functions
                #:make-stepper
                #:seconds)
  (:import-from #:arrow-macros
                #:->)
  (:import-from #:alexandria
                #:maphash-values
                #:maphash-keys
                #:hash-table-keys
                #:positive-fixnum
                #:emptyp
                #:when-let
                #:when-let*
                #:if-let
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:serapeum
                #:op
                #:do-each
                #:vect
                #:random-in-range
                #:class-name-of))

(in-package #:scenic)

(defstruct-g assimp-bones
  (ids     :vec4)
  (weights :vec4))

(defvar *point-light-params*
  (list (v! 3250 0.0014 0.000007)
        (v!  600 0.007  0.0002)
        (v!  325 0.014  0.0007)
        (v!  200 0.022  0.0019)
        (v!  160 0.027  0.0028)
        (v!  100 0.045  0.0075)
        (v!   65 0.07   0.017)
        (v!   50 0.09   0.032)
        (v!   32 0.14   0.07)
        (v!   20 0.22   0.20)
        (v!   13 0.35   0.44)
        (v!    7 0.7    1.8))
  "Length=12
   X=Distance
   Y=Linear
   Z=Quadratic")

;; https://docs.unity3d.com/ScriptReference/Vector3.html
(defvar *vec3-zero*    (v!  0  0  0))
(defvar *vec3-right*   (v!  1  0  0))
(defvar *vec3-left*    (v! -1  0  0))
(defvar *vec3-up*      (v!  0  1  0))
(defvar *vec3-down*    (v!  0 -1  0))
(defvar *vec3-forward* (v!  0  0  1))
(defvar *vec3-back*    (v!  0  0 -1))

;; https://lispcookbook.github.io/cl-cookbook/clos.html
(defclass counted-class (standard-class)
  ((counter :initform 0)))
(defmethod closer-mop:validate-superclass ((class counted-class) (superclass standard-class)) t)
(defmethod make-instance :after ((class counted-class) &key)
  (incf (slot-value class 'counter)))


(defvar *quad-stream-v3-data*
  (list (v! -1.0   1.0 0)
        (v! -1.0  -1.0 0)
        (v!  1.0  -1.0 0)
        (v! -1.0   1.0 0)
        (v!  1.0  -1.0 0)
        (v!  1.0   1.0 0)))

(let ((stream nil))
  (defun get-quad-stream-v3 ()
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (unless stream
      (setf stream (make-buffer-stream
                    (make-gpu-array *quad-stream-v3-data*
                                    :element-type :vec3
                                    :dimensions 6)
                    :retain-arrays t)))
    stream))

(defun random-v3 (&key (min 0f0) (max 1f0))
  (v! (serapeum:random-in-range min max)
      (serapeum:random-in-range min max)
      (serapeum:random-in-range min max)))

;; Took from shinmera/trial/render-loop.lisp
#+sbcl
(define-symbol-macro current-time-start
    (load-time-value (logand (sb-ext:get-time-of-day) (1- (expt 2 32)))))

(declaim (inline current-time))
(defun current-time ()
  (declare (optimize speed (safety 0)))
  #+sbcl (multiple-value-bind (s ms) (sb-ext:get-time-of-day)
           (let* ((s (logand s (1- (expt 2 62))))
                  (ms (logand ms (1- (expt 2 62)))))
             (declare (type (unsigned-byte 62) s ms))
             (+ (- s current-time-start)
                (* ms
                   (coerce 1/1000000 'double-float)))))
  #-sbcl (* (get-internal-real-time)
            (coerce (/ internal-time-units-per-second) 'double-float)))
