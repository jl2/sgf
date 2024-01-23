
sgf: manifest.txt *.lisp *.asd
	buildapp --output sgf \
             --manifest-file ~/src/lisp/sgf/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgf\
             --entry 'sgf:main'

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/sgf/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean
