OS ?= Linux

AppDir/scenic:
	sbcl --non-interactive \
	--eval '(load "~/quicklisp/setup.lisp")' \
	--eval '(ql:quickload :deploy)' \
	--load ../scenic.asd \
	--eval '(ql:quickload :scenic)' \
	--eval '(deploy:define-library CL-OPENGL-BINDINGS::OPENGL :dont-deploy t)' \
	--eval '(asdf:make :scenic :force t)'
ifeq ($(OS),Linux)
	find . -name '*.so*' -exec strip {} \;
	rm -vf AppDir/libstdc* # NOTE: bodge-ode dep
	rm -vf AppDir/libgcc*
endif

AppDir/static:
	install -d AppDir/static/SFX/Step
	install -D ../static/bunny.obj          AppDir/static/bunny.obj
	install -D ../static/SFX/Step/Step*.wav AppDir/static/SFX/Step

scenic.AppImage: AppDir/scenic AppDir/static AppRun iconfile.svg scenic.desktop appimagetool
	install -D scenic.desktop AppDir/scenic.desktop
	install -D AppRun         AppDir/AppRun
	install -D iconfile.svg   AppDir/iconfile.svg
	ARCH=x86_64 ./appimagetool -v AppDir scenic.AppImage

scenic-$(OS).tgz: AppDir/scenic AppDir/static
	tar cvzf scenic-$(OS).tgz AppDir/ --transform s/AppDir/scenic/

.PHONY: tar
tar: scenic-$(OS).tgz

.PHONY: clean
clean:
	rm -vrf AppDir/
	rm -vf scenic-$(OS).tgz
	rm -vf scenic.AppImage

appimagetool:
	wget -O appimagetool 'https://github.com/AppImage/AppImageKit/releases/download/12/appimagetool-x86_64.AppImage'
	chmod +x appimagetool
