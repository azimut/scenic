(asdf:defsystem #:scenic
  :description "Describe scenic here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:alexandria
               #:classimp
               #:arrow-macros
               #:bodge-ode
               #:ode-blob
               #:cepl
               #:cepl.sdl2
               #:cepl.skitter.sdl2
               #:dirt
               #:log4cl
               #:livesupport
               #:nineveh
               #:nepal
               #:cepl.fond
               #:fare-memoization
               ;;#:png
               #:rocketman
               #:rtg-math.vari
               #:serapeum
               #:temporal-functions
               #:with-setf)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "AppDir/scenic"
  :entry-point "scenic:start"
  :components
  ((:file "package")
   (:file "uploadable")
   (:file "event-loop")
   (:file "skitter")
   (:file "rocketman")
   ;;(:file "screenshot")
   (:file "assets")
   (:file "renderable")
   (:file "camera/camera")
   (:file "camera/defered")
   (:file "scene")
   (:file "scene-ibl")
   (:file "screen")
   (:file "state")
   (:file "actors/drawable")
   (:file "actors/occluder")
   (:file "actors/paintable")
   (:file "actors/actor")
   (:file "material/lambert")
   (:file "material/pbr")
   (:file "material/material")
   (:file "lights/colors")
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
   (:file "scenic")
   (:file "postprocess/postprocess")
   (:file "postprocess/simple")
   (:file "postprocess/dither")
   (:file "postprocess/defer/defer")
   (:file "postprocess/defer/ibl")
   (:file "postprocess/dof")
   (:file "postprocess/fog/iqfog")
   (:file "actors/untextured/untextured")
   (:file "actors/untextured/defer")
   (:file "actors/untextured/forward")
   (:file "actors/untextured/forward-ibl")
   (:file "actors/textured/textured")
   ;;(:file "actors/textured/forward")
   (:file "actors/textured/defer")
   (:file "actors/textured/assimp")
   (:file "actors/assimp/bones-helpers")
   (:file "actors/assimp/otherbones")
   (:file "actors/assimp/main")
   (:file "animated")
   (:file "actors/particles/base")
   (:file "actors/particles/particles")
   (:file "actors/particles/billboards")
   (:file "actors/text/font")
   (:file "actors/text/billboard")
   (:file "actors/text/screen")
   (:file "actors/cube/cube")
   (:file "actors/cube/hdri")
   (:file "actors/cube/sky")
   (:file "ode/ode")
   (:file "ode/derived")
   (:file "ode/scene")
   (:file "ode/raycast")
   (:file "ode/physic")
   (:file "ode/camera")
   (:file "ode/physics/box")
   (:file "ode/physics/sphere")
   (:file "ode/physics/cone")
   (:file "ode/physics/newcyl")
   (:file "register")
   (:file "audio/nepal")
   (:file "audio/emitter")
   (:file "audio/sfx")
   (:file "controls/control")
   (:file "controls/ode")
   (:file "controls/rotation")
   (:file "controls/mouse")
   (:file "demo")))
