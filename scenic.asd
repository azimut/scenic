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
               #:rocketman
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
   (:file "rocketman")
   (:file "screenshot")
   (:file "assets")
   (:file "renderable")
   (:file "camera/camera")
   (:file "camera/defered")
   (:file "state")
   (:file "scene")
   (:file "control")
   (:file "actors/actor")
   (:file "lights/light")
   (:file "lights/directional")
   (:file "lights/point")
   (:file "lights/spot")
   (:file "lights/lights")
   (:file "lights/ibl/brdf")
   (:file "lights/ibl/capture")
   (:file "lights/ibl/irradiance")
   (:file "lights/ibl/prefilter")
   (:file "lights/ibl/ibl")
   (:file "render")
   (:file "material/lambert")
   (:file "material/pbr")
   (:file "material/material")
   (:file "scenic")
   (:file "postprocess/postprocess")
   (:file "postprocess/simple")
   (:file "postprocess/defer")
   (:file "actors/actors")
   (:file "actors/untextured")
   (:file "actors/untextured-defer")))
