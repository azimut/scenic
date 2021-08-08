zip:
	cd bin/ && zip -r ../scenic.zip .

clean:
	rm -v -f ./*.zip
	rm -v -f ./bin/*.so*
	rm -v -f ./bin/scenic

strip:
	find . -name '*.so*' -exec strip {} \;

build:
	/usr/local/bin/sbcl \
		--eval '(ql:quickload :scenic)' \
		--eval '(asdf:make :scenic)'

dump: clean build strip

run:
	./scenic
