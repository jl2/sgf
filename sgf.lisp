;; sgf.lisp
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

(in-package :sgf)


(defparameter *sgf-file-dirs* (list
                               (asdf:system-relative-pathname :sgf "sgf-files/")
                               "~/data/go-games/"
                               (asdf:system-relative-pathname :sgf "examples/"))
  "Directories in which to look for game files.")

(defun find-sgf-file (fname)
  "Search for a  name (like \"game1.sgf\" in th *sgf-file-dires*"
  (ctypecase fname
    (string
     (if (probe-file fname)
         fname
         (loop
           :for path :in *sgf-file-dirs*
           :do
              (when-let (the-file (probe-file (merge-pathnames fname path)))
                (return (merge-pathnames fname path)))
           :finally (error "Could not find ~a" fname))))
    (pathname
     (probe-file fname))))

(declaim (inline read-sgf))
(defgeneric read-sgf (path-or-name-or-stream)
  (:documentation "Read an SGF file from a path, a stream, or a game file name from *sgf-file-dires*"))

(defmethod read-sgf ((name string))
  (read-sgf (find-sgf-file name)))

(defmethod read-sgf ((path pathname))
  (read-sgf-collection (create-parser
                        (alexandria:read-file-into-string path))))
(defmethod read-sgf ((stream stream))
  (read-sgf-collection
   (create-parser
    (alexandria:read-stream-content-into-string stream))))


(defun whitespace-p (char)
  "Return t if a character is a whitespace, nil otherwise."
  (declare (type character char))
  #+sbcl (sb-unicode:whitespace-p char)
  #-sbcl (cl-unicode:has-property char "whitespace"))

(defstruct (sgf-parser (:conc-name sp-))
  "A SGF file parser."
  (text "" :type string)
  (cur-pos 0 :type fixnum))

(defmethod print-object ((object sgf-parser) stream)
  "Try to pretty print an sgf-parser"
  (with-slots (cur-pos text) object
    (declare (type fixnum cur-pos)
             (type string text)
             (type stream stream))
    
    (format stream "(sgf-parser: pos: ~a text: ~s"
            cur-pos
            (subseq text
                    (max (- cur-pos 5) 0)
                    (min (+ cur-pos 5) (length text))))))

(defun create-parser (my-string)
  "Create an SGF parser for a string."
  (make-sgf-parser :text my-string))

(defun finished-p (parser)
  "t if the parser has read the whole string.  nil otherwise"
  (with-slots (text cur-pos) parser
    (declare (type fixnum cur-pos)
             (type string text))
    (= cur-pos (length text))))

(defun current-char (parser)
  "Character currently being read."
  (with-slots (text cur-pos) parser
    (declare (type fixnum cur-pos)
             (type simple-string text))
    (loop
      :while (and
              (not (finished-p parser))
              (whitespace-p (aref text cur-pos)))
      :when (finished-p parser)
        :do (return nil)
      :do (incf cur-pos)
      :finally (return (aref text cur-pos)))))

(defun read-until (parser char)
  "Return a string from cur-pos until the first instance of char, and advance the parser."
  (with-slots (text cur-pos) parser
    (declare (type fixnum cur-pos)
             (type string text)
             (type character char))
    (let* ((idx (position char text :start cur-pos))
           (str (subseq text
                        cur-pos
                        idx)))
      (setf cur-pos idx)
      str)))


(defun match (parser char)
  "Validate that the character at cur-pos matches char and advance the parser."
  (when (char/= (current-char parser) char)
    (error "Expected ~a but found ~a" (current-char parser) char))
  (incf (sp-cur-pos parser)))

;; These functions are based on the EBNF from here: https://www.red-bean.com/sgf/sgf4.html

(defun read-sgf-collection (parser)
  "Read an SGF collection."
  (loop :for game-tree = (read-sgf-gametree parser)
        :while game-tree
        :collecting game-tree))

(defun read-sgf-gametree (parser)
  "Read an SGF Game Tree"
  (if (finished-p parser)
      nil
      (case (current-char parser)
        (nil nil)
        (#\(
         (match parser #\( )
         (prog1
             (nconc (read-sgf-sequence parser)
                    (read-sgf-gametree parser))
           (match parser #\))))
        (otherwise nil))))

(defun read-sgf-sequence (parser)
  "Read an SGF sequence."
  (nconc
   (loop :for node = (read-sgf-node parser)
         :while node
         :nconcing node)
   (case (current-char parser)
     (#\(
      (loop :for game-tree = (read-sgf-gametree parser)
            :while game-tree
            :nconcing game-tree))
     (otherwise nil))))

(defun read-sgf-node (parser)
  "Read an SGF node (property list)"
  (case (current-char parser)
    (nil nil)
    (#\) nil)
    (#\;
     (match parser #\;)
     (loop :for property = (read-sgf-property parser)
           :while property
           :collecting property))))

(defun read-sgf-property (parser)
  "Read an SGF property."
  (case (current-char parser)
    (nil nil)
    (#\( (read-sgf-sequence parser))
    (#\) nil)
    (#\; (read-sgf-node parser))

    (otherwise 
     (let ((node-tag (read-until parser #\[)))
       (match parser #\[)
       (let ((text (read-until parser #\])))
         (match parser #\])
         (cons node-tag text))))))
