(in-package #:bio-utils-test)


(lisp-unit2:define-test utils-test
    ()
  (let* ((to-test "Ala, ma, kota, Kosika")
	 (splited (bio-utils::string-list to-test #\,)))
    (format t "~A~%list size: ~d~%" splited (length splited))))
