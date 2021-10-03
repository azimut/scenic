@PHONY: zip clean run

scenic: clean
	sbcl --eval '(asdf:make :scenic)'
	find . -name '*.so*' -exec strip {} \;

zip:
	zip -r ../scenic.zip .

clean:
	rm -v -f ../scenic.zip
	rm -v -f ./*.so*
	rm -v -f ./scenic

run:
	LD_PRELOAD="$(shell echo ./*.so*)" ./scenic
