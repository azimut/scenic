(in-package #:scenic)

(defclass envmap (cube)
  ()
  (:documentation "textured RGB8 cubemap, from 6 images"))

(defun cubes-sam-from-images (&rest paths)
  "Returns a gpu texture FROM the provided images"
  (assert (= 6 (length paths)))
  (log4cl:log-info "loading CUBEMAP texture")
  (with-c-arrays-freed
      (ca (mapcar
           (lambda (p)
             (dirt:load-image-to-c-array
              (asdf:system-relative-pathname :scenic p)))
           paths))
    (serapeum:~> (make-texture ca :element-type :rgb8 :cubes t)
                 (sample :wrap :clamp-to-edge :magnify-filter :linear))))

(defun make-envmap (left right up down front back
                    &rest args
                    &aux (skey (list left right up down front back)))
  (let ((sam (or (gethash skey *samplers*)
                 (setf (gethash skey *samplers*)
                       (cubes-sam-from-images left right up down front back)))))
    (apply #'make-instance 'envmap :sam sam args)))

(defmethod paint (scene camera (obj envmap) time)
  (with-slots (buf sam color gamma) obj
    (map-g #'cube-pipe buf
           :gamma gamma
           :sam sam
           :color color
           :view (q:to-mat4 (q:inverse (rot camera)))
           :proj (projection camera))))
