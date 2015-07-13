
(defpackage #:bio-utils-test
  (:use :common-lisp :bio-utils :lisp-unit2))


(in-package #:bio-utils-test)

(lisp-unit2:define-test sgd-simple-test
    ()
    (get-sgd "YPL252C"))
