(in-package #:scenic)

(defclass animated ()
  ((animations :reader   animations)
   (current    :accessor current    :initarg :current)
   (clock      :accessor clock      :initarg :clock)
   start
   end
   (inc        :reader inc)
   loop-p
   index)
  (:default-initargs
   :current (error "select a CURRENT animation")
   :clock 0f0)) ;; DOUBLE?

(defmethod initialize-instance :after ((obj animated) &key current)
  (setf (current obj) current))

(defmethod (setf current) :before (new-value (obj animated))
  (assert (assoc new-value (animations obj))))
(defmethod (setf current) :after (new-value (obj animated))
  (destructuring-bind (&key inc start end loop-p index) (serapeum:assocdr new-value (animations obj))
    (setf (slot-value obj 'loop-p) loop-p)
    (setf (slot-value obj 'index)  index)
    (setf (slot-value obj 'start)  start)
    (setf (slot-value obj 'end)    end)
    (setf (slot-value obj 'inc)    inc)))

(if loop-p
    (setf clock start)
    (setf current default))

(defmethod update :after ((obj animated) dt)
  (incf (clock obj) (inc obj)))

;; :inc 0.12
;; :ticks-per-second 20.0d0
;; :clock
(serapeum:assocdr
 :walking '((:walking
             :animation 0
             :loop-p T
             :start 10f0
             :end   20f0)
            (:running
             :animation 0
             :loop-p T
             :start 0f0
             :end   NIL)))
