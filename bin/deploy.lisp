(when (probe-file "~/quicklisp/setup.lisp")
  (load "~/quicklisp/setup.lisp"));; NOT NEEDED FOR QLOT

(ql:quickload :deploy)
(load "scenic.asd")
(ql:quickload :scenic)

(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)
(deploy:define-resource-directory assets "static/")
(deploy:define-hook (:quit scenic) ()
  (alut:exit)
  (cepl:quit))

(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (mapc #'asdf:register-immutable-system (asdf:already-loaded-systems))
  #+asdf (setf asdf:*central-registry* NIL)
  #+quicklisp (setf ql:*local-project-directories* ())
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))

(sb-ext:gc :full t)
(asdf:make :scenic :force t)
