.PHONY: tar clean run strip

scenic:
	sbcl --disable-debugger \
             --eval '(load "~/quicklisp/setup.lisp")' \
             --eval '(ql:quickload :deploy)' \
             --load ../scenic.asd \
             --eval '(ql:quickload :scenic)' \
             --eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
             --eval '(asdf:make :scenic :force t)' \
             --quit
	rm -v -f libSDL2* # NOTE: we probably wan to keep the .DLL on Windows
	rm -v -f libstdc* # NOTE: bodge/claw bug?

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
