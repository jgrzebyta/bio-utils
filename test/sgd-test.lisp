;; To run single test do:
;; (lisp-unit2:run-test 'bio-utils-test::<test name>) eg: (lisp-unit2:run-test 'bio-utils-test::sdb-ykl132c-test)


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


(lisp-unit2:define-test sdb-YKL132C-test
    ()
  (let* ((result (get-jso-str "YKL132C")))
    (format t "data-str: ~%~a~%" result)
    ))


;; Additional utils functions 

(defun get-jso-str (name)
  "Return JDO record identified by NAME."
  (let* ((base-url (format nil "http://www.yeastgenome.org/search?query=~a" name))
	 (page (drakma:http-request base-url))
	 (json-string (bio::--get-bootstrap-json-as-string page))
	 (json-object (st-json:read-json-from-string json-string)))
    json-object))


