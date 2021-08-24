(uiop:define-package #:scenic
  (:use #:cepl
        #:cepl.skitter
        #:cl
        #:livesupport
        #:nineveh
        #:rtg-math
        #:vari
        #:with-setf)
  (:export #:start)
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
