OS ?= Linux

AppDir/scenic:
	sbcl --non-interactive --load ./deploy.lisp
ifeq ($(OS),Linux)
	find . -name '*.so*' -exec strip {} \;
	rm -vf AppDir/libstdc* # NOTE: bodge-ode dep
	rm -vf AppDir/libgcc*
endif
	install -D ../LICENSE   AppDir/
	install -D ../README.md AppDir/

scenic.AppImage: AppDir/scenic AppRun iconfile.svg scenic.desktop appimagetool
	install -D scenic.desktop AppDir/scenic.desktop
	install -D AppRun         AppDir/AppRun
	install -D iconfile.svg   AppDir/iconfile.svg
	ARCH=x86_64 ./appimagetool -v AppDir scenic.AppImage

scenic-$(OS).tgz: AppDir/scenic
	tar cvzf scenic-$(OS).tgz AppDir/ --transform s/AppDir/scenic-$(OS)/

scenic-$(OS).zip: AppDir/scenic
	mv AppDir scenic-$(OS)
ifeq ($(OS),Windows_NT)
	Compress-Archive -Path scenic-$(OS) -Destination scenic-$(OS).zip
else
	zip -X -r scenic-$(OS).zip scenic-$(OS)
endif

.PHONY: tar
tar: scenic-$(OS).tgz

.PHONY: zip
zip: scenic-$(OS).zip

.PHONY: clean
clean:
	rm -vrf AppDir/
	rm -vf scenic-$(OS).tgz
	rm -vf scenic.AppImage

appimagetool:
	wget -O appimagetool 'https://github.com/AppImage/AppImageKit/releases/download/12/appimagetool-x86_64.AppImage'
	chmod +x appimagetool
