OS ?= Linux

AppDir/scenic:
ifeq ($(OS),Windows_NT)
	sbcl --non-interactive \
             --eval '(load "~/quicklisp/setup.lisp")' \
             --eval '(ql:quickload :deploy)' \
             --load ../scenic.asd \
             --eval '(ql:quickload :scenic)' \
             --eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
             --eval '(asdf:make :scenic :force t)'
else
	sbcl --non-interactive \
             --eval '(load "~/quicklisp/setup.lisp")' \
             --eval '(ql:quickload :deploy)' \
             --load ../scenic.asd \
             --eval '(ql:quickload :scenic)' \
             --eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
             --eval '(asdf:make :scenic :force t)'
	rm -f AppDir/libstdc* # NOTE: bodge-ode dep
	rm -f AppDir/libgcc*
endif

scenic.AppImage: AppDir/scenic AppRun iconfile.svg scenic.desktop
	install -D scenic.desktop AppDir/scenic.desktop
	install -D AppRun         AppDir/AppRun
	install -D iconfile.svg   AppDir/iconfile.svg
	wget -c -O ~/Downloads/appimagetool https://github.com/AppImage/AppImageKit/releases/download/12/appimagetool-x86_64.AppImage
	ARCH=x86_64 ~/Downloads/appimagetool -v AppDir scenic.AppImage

.PHONY: tar
tar: scenic-$(OS).tgz
scenic-$(OS).tgz: AppDir/scenic
	find . -name '*.so*' -exec strip {} \;
	tar cvzf scenic-$(OS).tgz AppDir/

PHONY: clean
clean:
	rm -rf AppDir/
	rm -f scenic-$(OS).tgz
	rm -f scenic.AppImage
