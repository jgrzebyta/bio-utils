
(defpackage #:bio-utils-test
  (:use :common-lisp :bio-utils :lisp-unit2))


(in-package #:bio-utils-test)

;; (lisp-unit2:define-test sgd-simple-test
;;     ()
;;  (format t "data str: ~%~a~%" (inspect (get-sgd "YPL252C"))))


(lisp-unit2:define-test sgd-simple-hashtable-test
    ()
  (let* ((record (get-sgd "YPL252C"))
	 (display-name (gethash "display-name" record))
	 (uniprot-id (gethash "uniprot-id" record)))
    (assert-true (not (null display-name)))
    (assert-true (not (null uniprot-id)))))


(lisp-unit2:define-test sgd-simple-key-test
    ()
  (let ((result (get-sgd "YPL252C")))
    (loop for k being the hash-keys of result
	 do (format t "hash key: ~s~%" k))
    ))
