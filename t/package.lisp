;; package.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :sgf.test
  (:use :cl
        :fiveam
        :alexandria
        :sgf))

(in-package :sgf.test)

(def-suite :sgf)
(in-suite :sgf)

(test sgf-finished
  (let ((parser (sgf::create-parser "")))
    (is-true (sgf::finished-p parser)))
  (let ((parser (sgf::create-parser "a[b]")))
    (is-false (sgf::finished-p parser))
    (sgf::read-sgf-property parser)
    (is-true (sgf::finished-p parser))
    (is-true (null (sgf::read-sgf-property parser)))))

(test sgf-property
  (let* ((parser (sgf::create-parser "a[bc]"))
         (prop (sgf::read-sgf-property parser)))
    (is-true (string= (car prop) "a"))
    (is-true (string= (cdr prop) "bc"))))


(test sgf-node-one-property
  (let* ((parser (sgf::create-parser ";a[bc]"))
         (node (sgf::read-sgf-node parser)))
    (is-true (=  (length node) 1))

    (let ((prop1 (car node)))
      (is-true (string= (car prop1) "a"))
      (is-true (string= (cdr prop1) "bc")))))

(test sgf-node-two-properties
  (let* ((parser (sgf::create-parser ";a[bc] x[yz]"))
         (node (sgf::read-sgf-node parser)))
    (is-true (=  (length node) 2))

    (let ((prop1 (car node))
          (prop2 (cadr node)))

      (is-true (string= (car prop1) "a"))
      (is-true (string= (cdr prop1) "bc"))
      (is-true (string= (car prop2) "x"))
      (is-true (string= (cdr prop2) "yz")))))
