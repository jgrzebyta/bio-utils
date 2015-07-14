
(defpackage #:bio-utils-test
  (:use :common-lisp :bio-utils :lisp-unit2))


(in-package #:bio-utils-test)

(lisp-unit2:define-test sgd-simple-test
    ()
 (format t "data str: ~%~a~%" (inspect (get-sgd "YPL252C"))))
