OS ?= Linux

AppDir/scenic:
	sbcl --non-interactive \
	--eval '(load "~/quicklisp/setup.lisp")' \
	--eval '(ql:quickload :deploy)' \
	--load ../scenic.asd \
	--eval '(ql:quickload :scenic)' \
	--eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
	--eval '(asdf:make :scenic :force t)'
ifneq ($(OS),Windows_NT)
	rm -vf AppDir/libstdc* # NOTE: bodge-ode dep
	rm -vf AppDir/libgcc*
endif

appimagetool:
	wget -O appimagetool 'https://github.com/AppImage/AppImageKit/releases/download/12/appimagetool-x86_64.AppImage'
	chmod +x appimagetool

scenic.AppImage: AppDir/scenic AppRun iconfile.svg scenic.desktop appimagetool
	install -D scenic.desktop AppDir/scenic.desktop
	install -D AppRun         AppDir/AppRun
	install -D iconfile.svg   AppDir/iconfile.svg
	mkdir -p AppDir/static
	install -D ../static/bunny.obj AppDir/static/bunny.obj
	ARCH=x86_64 ./appimagetool -v AppDir scenic.AppImage

scenic-$(OS).tgz: AppDir/scenic
	find . -name '*.so*' -exec strip {} \;
	tar cvzf scenic-$(OS).tgz AppDir/

.PHONY: tar
tar: scenic-$(OS).tgz

.PHONY: clean
clean:
	rm -vrf AppDir/
	rm -vf scenic-$(OS).tgz
	rm -vf scenic.AppImage
