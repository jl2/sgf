
sgf: manifest.txt t/*.lisp *.lisp *.asd
	buildapp --output sgf \
             --manifest-file ~/src/lisp/sgf/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgf \
             --entry 'sgf:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :sgf.test)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"


manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:quickload :sgf.test)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/sgf/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean
