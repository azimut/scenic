(asdf:defsystem #:scenic
  :description "Describe scenic here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:alexandria
               #:arrow-macros
               #:cepl
               #:cepl.sdl2
               #:cepl.skitter.sdl2
               #:dirt
               #:log4cl
               #:livesupport
               #:nineveh
               #:png
               #:rtg-math.vari
               #:serapeum
               #:temporal-functions
               #:with-setf)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "scenic"
  :entry-point "scenic:start"
  :components
  ((:file "package")
   (:file "assets")
   (:file "camera")
   (:file "misc-gpu")
   (:file "render")
   (:file "scene")
   (:file "lights/lights")
   (:file "lights/light")
   (:file "lights/directional")
   (:file "lights/point")
   (:file "scenic")
   (:file "postprocess")
   (:file "actors")))
