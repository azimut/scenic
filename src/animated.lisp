(in-package #:scenic)

(defclass animated ()
  ((animations :reader   animations :initarg :animations)
   (current    :accessor current    :initarg :current)
   (clock      :accessor clock      :initform 1f0) ;; DOUBLE?
   start
   end
   (inc        :reader inc)
   (loop-p     :reader loop-p)
   (index      :initarg :index))
  (:default-initargs
   :index 0
   :current :idle
   :animations '((:idle)))
  (:documentation "inherith to have a CLOCK tick by INC and select CURRENT between different ANIMATIONS"))

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

(defun return-to-default (obj)
  (setf (current obj) (first (first (animations obj)))))

(defmethod (setf clock) :around (new-value (obj animated))
  (with-slots (start end loop-p) obj
    (cond ((and (not loop-p) (> new-value end)) ;; FIXME: what a mess!!
           (return-to-default obj))
          ((and loop-p (> new-value end))
           (call-next-method start obj))
          (t (call-next-method)))))

(defmethod update :after ((obj animated) dt)
  (when-let ((inc (inc obj)))
    (incf (clock obj) inc)))

