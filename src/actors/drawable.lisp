(in-package #:scenic)

(defclass drawable ()
  ((stepper  :reader stepper    :initarg :stepper
             :documentation "the stepper")
   (interval :accessor interval :initarg :interval
             :documentation "controls how often we draw, optional")
   (drawp    :accessor drawp    :initarg :drawp
             :documentation "should be drawn?")
   (shadowp  :accessor shadowp  :initarg :shadowp
             :documentation "casts shadow?"))
  (:default-initargs
   :interval -1
   :drawp T
   :shadowp T))

(defmethod initialize-instance
    :before ((obj drawable) &key interval)
  (check-type interval number))

(defmethod initialize-instance
    :after ((obj drawable) &key interval)
  (when (plusp interval)
    (setf (slot-value obj 'stepper)
          (make-stepper (seconds interval) (seconds interval)))))

(defmethod draw :around (scene obj time)
  (with-slots (drawp interval stepper) obj
    (when (and drawp (or (minusp interval)
                         (funcall stepper)))
      (call-next-method))))
