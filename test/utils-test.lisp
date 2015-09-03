;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:bio-utils-test)
(require 'bio-utils)
(require 'lisp-unit2)

(lisp-unit2:define-test utils-test
    ()
  (let* ((to-test "Ala, ma, kota, Kosika")
	 (splited (bio-utils::string-list to-test #\,)))
    (format t "~A~%list size: ~d~%" splited (length splited))))

(defvar *multiline-text* "Ala, ma, kotka
Zabka, malego, lakotka")

(defvar *multiline-text2* "Ala, ma, kotka
Zabka, malego, lakotka

Jedzie, Jasiu, na, kobyle")

(defun load-file-to-string (input-file)
  (let ((input-data-stream (open input-file :if-does-not-exist :error))
	(out-string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (do ((c (read-char input-data-stream nil) (read-char input-data-stream nil)))
	((null c))
      (vector-push-extend c out-string))
    out-string))

(lisp-unit2:define-test multiline-string-test
    ()
  (let ((result (bio-utils::multiline-string-list *multiline-text* #\,)))
    (format t "1st size ~d ~%" (length result))
    (assert-number-equal 2 (length result))
    (loop for l in result
       do (loop for word in l do
	       (format t "~S~%" word)))))


(lisp-unit2:define-test multiline-with-empty-test
    ()
  (let* ((result (bio-utils::multiline-string-list *multiline-text2* #\,))
	 (last-line (nth 2 result))
	 (word (nth 3 last-line)))
    (assert-number-equal 3 (length result))
    (assert-equal "kobyle" word)
    (format t "result: ~S~% with size: ~d~%" result (length result))
    (format t "last line: ~S~%" last-line)
    (format t "element: ~S~%" word)))

(lisp-unit2:define-test clean-citation-chars
    ()
  (let ((is-1 "Ale tekst")
	(is-2 "\"Ale tekst\"")
	(is-3 "\'Ale tekst\'"))
    (lisp-unit2:assert-eql (length is-1) (length (bio-utils::string-remove-citations is-1)))
    (lisp-unit2:assert-eql (length is-1) (length (bio-utils::string-remove-citations is-2)))
    (lisp-unit2:assert-eql (length is-1) (length (bio-utils::string-remove-citations is-3)))
    (format t "string before: ~S string after: ~S string expected: ~S~%" is-2 (bio-utils::string-remove-citations is-2) is-1)))
