# sgf

### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_
This is a package to read "[Simple Game Format](https://www.red-bean.com/sgf/sgf4.html)" (SGF) files used
by many computerized Go programs.

This library doesn't interpret the data, but reads it into a Lisp data structure that can be easily manipulated.

I still need to use the library a bit and decide what type of interface to build on top.  Maybe the tree is easy
enough to use that `first`, `rest`, and `assoc` are good enough, but I may create structures to hold everything.

```
(ql:quickload :sgf)
(sgf:read-sgf "sample1.sgf")
(((("FF" . "4") ("C" . "root")) ((("C" . "a")) (("C" . "b")) ((("C" . "c"))) ((("C" . "d")) (("C" . "e"))))
  ((("C" . "f")) ((("C" . "g")) (("C" . "h")) (("C" . "i"))) ((("C" . "j"))))))
```
## License

ISC

Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


