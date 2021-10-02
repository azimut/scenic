(in-package #:scenic)

(defclass defered (renderable pers)
  ()
  (:default-initargs
   :texture-opts
   '((0  :element-type :rgba16f); color    roughness
     (1  :element-type :rgba32f); pos      ao
     (2  :element-type :rgba16f); norm     specular
     (3  :element-type :rg16f)  ; metallic emissive
     (:d :element-type :depth-component24))
   :sample-opts
   '((:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)
     (:wrap :clamp-to-edge)))
  (:documentation "defered perspective renderable camera"))

(defun make-defered (&rest args)
  (apply #'make-instance 'defered args))

