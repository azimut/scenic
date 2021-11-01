.PHONY: tar clean run strip

scenic:
ifeq ($(OS),Windows_NT)
	sbcl --non-interactive \
             --eval '(load "~/quicklisp/setup.lisp")' \
             --eval '(ql:quickload :deploy)' \
             --load ../scenic.asd \
             --eval '(ql:quickload :scenic)' \
             --eval '(asdf:make :scenic :force t)'
else
	sbcl --non-interactive \
             --eval '(load "~/quicklisp/setup.lisp")' \
             --eval '(ql:quickload :deploy)' \
             --load ../scenic.asd \
             --eval '(ql:quickload :scenic)' \
             --eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
             --eval '(asdf:make :scenic :force t)'
	rm -v -f libstdc* # NOTE: bodge-ode (?) dep
	rm -v -f libgcc*
endif
strip:
	find . -name '*.so*' -exec strip {} \;

tar:
	tar cvf ../scenic.tar ../bin --transform s/bin/scenic/

clean:
	rm -v -f ../scenic.tar
	rm -v -f ./*.so*
	rm -v -f ./scenic

run:
	LD_PRELOAD="$(shell echo ./*.so*)" ./scenic
