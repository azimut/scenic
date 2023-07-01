(load "~/quicklisp/setup.lisp")

(ql:quickload :deploy)
(load "../scenic.asd")
(ql:quickload :scenic)

(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)
(deploy:define-resource-directory assets "static/")
(deploy:define-hook (:quit scenic) ()
  (alut:exit)
  (cepl:quit))

(sb-ext:gc :full t)
(asdf:make :scenic :force t)
