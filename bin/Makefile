.PHONY: zip clean run strip

scenic:
	sbcl --disable-debugger \
             --eval '(load "~/quicklisp/setup.lisp")' \
             --eval '(ql:quickload :deploy)' \
             --load ../scenic.asd \
             --eval '(ql:quickload :scenic)' \
             --eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
             --eval '(asdf:make :scenic :force t)' \
             --quit

strip:
	find . -name '*.so*' -exec strip {} \;

zip:
	zip -r ../scenic.zip .

clean:
	rm -v -f ../scenic.zip
	rm -v -f ./*.so*
	rm -v -f ./scenic

run:
	LD_PRELOAD="$(shell echo ./*.so*)" ./scenic
