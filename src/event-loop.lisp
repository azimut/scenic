#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:scenic)

;; Copied from Shirakumo/trial

(defclass event ()
  ())

(defclass listener ()
  ())

(defgeneric add-listener (listener event-loop))
(defgeneric remove-listener (listener event-loop))
(defgeneric handle (event listener))

#-elide-handler-restarts
(defmethod handle :around ((event event) listener)
  (with-simple-restart (abort "Don't handle ~a in ~a." event listener)
    (call-next-method)))

;; Default to doing nothing.
(defmethod handle ((event event) (listener listener)))

(defclass event-loop ()
  ((queue :initform (make-array 64 :initial-element NIL :adjustable T :fill-pointer 0) :reader queue)
   (queue-index :initform 0 :accessor queue-index)
   (listeners :initform (make-hash-table :test 'eq) :accessor listeners)
   (listener-queue :initform '(NIL) :accessor listener-queue)
   (uuid :initform () :accessor uuid)))

(defun issue (loop event-type &rest args)
  (let ((event (etypecase event-type
                 (event event-type)
                 ((or class symbol)
                  (apply #'make-instance event-type args))))
        (uuid  (list* event-type args)))
    (when (not (position uuid (uuid loop) :test #'equal))
      (push uuid (uuid loop))
      (vector-push-extend event (queue loop)))))

(define-compiler-macro issue (&environment env loop event-type &rest args)
  (cond ((and (constantp event-type env)
              (listp event-type)
              (eql (first event-type) 'quote)
              (symbolp (second event-type)))
         `(vector-push-extend (make-instance ,event-type ,@args) (queue ,loop)))
        (T
         (let ((eventg (gensym "EVENT")))
           `(let* ((,eventg ,event-type)
                   (,eventg (etypecase ,eventg
                              (event ,eventg)
                              ((or class symbol)
                               (make-instance ,eventg ,@args)))))
              (vector-push-extend ,eventg (queue ,loop)))))))

;; FIXME: This will forget events if PROCESS or DISCARD-EVENTS is called
;;        recursively (thus resetting the index) and new events are issued
;;        beyond the point of the index where the recursive call happens.
;;        The check will assume nothing has changed and it'll continue from
;;        where it left off, thus missing events before the current index.
(defmethod process ((loop event-loop))
  (declare (optimize speed))
  (with-simple-restart (discard-events "Discard all events.")
    (let ((queue (queue loop)))
      (declare (type (and (vector T) (not simple-array)) queue))
      (loop for i = (1- (the (unsigned-byte 32) (incf (queue-index loop))))
            while (< i (length queue))
            do (let ((event (aref queue i)))
                 (when event
                   (handle event loop)
                   (setf (aref queue i) NIL))))
      (setf (fill-pointer queue) 0
            (queue-index loop) 0)
      (setf (uuid loop) '()))))

(defun discard-events (loop)
  (loop for i = (1- (incf (queue-index loop)))
        while (< i (length (queue loop)))
        do (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0
        (queue-index loop) 0))

(defmethod handle ((event event) (loop event-loop))
  (with-simple-restart (skip-event "Skip handling the event entirely.")
    (loop with queue = (listener-queue loop)
          for listener = (pop queue)
          while listener
          do (handle event listener))))

(defmethod add-listener (listener (loop event-loop))
  (if (gethash listener (listeners loop))
      listener
      (let ((cons (cons listener (listener-queue loop))))
        (setf (gethash listener (listeners loop)) cons)
        (setf (listener-queue loop) cons)
        listener)))

(defmethod remove-listener (listener (loop event-loop))
  (let* ((listeners (listeners loop))
         (cons (gethash listener listeners)))
    (declare (type hash-table listeners))
    (when cons
      (setf (car cons) (cadr cons))
      (setf (cdr cons) (cddr cons))
      (setf (gethash (car cons) listeners) cons))
    (remhash listener listeners)
    listener))

(defmethod free :after ((loop event-loop))
  (discard-events loop)
  (clrhash (listeners loop))
  (setf (listener-queue loop) '(NIL)))

(defun enlist (item &rest items)
  (if (listp item) item (list* item items)))

(defmacro define-handler ((class event &rest qualifiers) slots &body body)
  (destructuring-bind (instance class) (enlist class class)
    (destructuring-bind (variable event) (enlist event event)
      `(defmethod handle ,@qualifiers ((,variable ,event) (,instance ,class))
         (let ,(loop for slot in slots
                     for (var name) = (enlist slot slot)
                     collect `(,var (slot-value ,variable ',name)))
           ,@body)))))

(defclass tick (event)
  ((tt :initarg :tt :accessor tt)
   (dt :initarg :dt :accessor dt)
   (fc :initarg :fc :accessor fc)))

(defclass class-changed (event)
  ((changed-class :initarg :changed-class :accessor changed-class)))
