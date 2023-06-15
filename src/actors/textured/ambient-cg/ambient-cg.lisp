(in-package #:scenic)

(defclass textured-ambient-cg (textured actor albedoed normaled displaced roughed)
  ()
  (:default-initargs
   :buf (box 1f0 1f0 1f0 t));; :buf needs tangents for PBR
  (:documentation "https://ambientcg.com/"))
