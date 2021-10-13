(in-package #:scenic)

(defclass font ()
  ((msg       :accessor msg       :initarg :msg)
   (path      :reader   path      :initarg :path)
   (blend     :reader   blend     :initarg :blend   :allocation :class)
   (charset   :reader   charset   :initarg :charset)
   (fond-font :reader   fond-font)
   (fond-text :reader   fond-text))
  (:default-initargs
   :msg "..."
   :blend (make-blending-params :source-alpha :one-minus-src-alpha)
   :path (error "missing font path")
   :charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ."))

(defmethod print-object ((obj font) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (path msg) obj
      (format stream "~s ~s" path msg))))

(defmethod (setf msg) :after (new-value (obj font))
  (cepl.fond:update-fond-text (fond-text obj) new-value))

(defmethod initialize-instance :before ((obj font) &key path)
  (assert (probe-file path))
  (assert (serapeum:string$= ".ttf" (string-downcase path))))
(defmethod initialize-instance :after ((obj font) &key msg path charset)
  (with-slots (fond-font fond-text) obj
    (setf fond-font (cepl.fond:make-fond-font path charset))
    (setf fond-text (cepl.fond:make-fond-text fond-font msg))))

(defmethod free :after ((obj font))
  (with-slots (fond-font fond-text) obj
    (cl-fond:free (cepl.fond::fond-font-font fond-font))
    (free (sampler-texture (cepl.fond::fond-font-sampler fond-font)))
    (free (cepl.fond::fond-text-stream fond-text))))
