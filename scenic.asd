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
   (:file "screenshot")
   (:file "assets")
   (:file "camera")
   (:file "render")
   (:file "material/render")
   (:file "material/pbr")
   (:file "state")
   (:file "scene")
   (:file "lights/light")
   (:file "lights/directional")
   (:file "lights/point")
   (:file "lights/lights")
   (:file "scenic")
   (:file "postprocess")
   (:file "actors")))
