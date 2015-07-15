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
    (format t "data: ~a ~a ~a ~%" record display-name uniprot-id)
    (assert-true (not (null display-name)))
    (assert-true (not (null uniprot-id)))))


(lisp-unit2:define-test sgd-simple-key-test
    ()
  (let ((result (get-sgd "YPL252C")))
    (loop for k being the hash-keys of result
	 do (format t "hash key: ~s~%" k))
    ))


(lisp-unit2:define-test go-YKL132C-test ()
  "Test GO terms for that ORF"
  (let* ((result (get-sgd "YKL132C"))
	 (go-terms (gethash "go-terms" result)))
    (assert-true (not (null go-terms)))
    (format t "keys:~%")
    (loop for k being the hash-keys in go-terms
       using (hash-value v)
	 do (format t "key: ~s value: ~a~%" k v))
    ))



(lisp-unit2:define-test go-YPL252C-test ()
  "Test GO terms for that ORF"
  (let* ((result (get-testing-jso))
	 (go-terms (gethash "go-terms" result)))
    (assert-true (not (null go-terms)))
    (format t "keys:~%")
    (loop for k being the hash-keys in go-terms
       using (hash-value v)
	 do (format t "key: ~s value: '~a'~%" k v))
    ))

(lisp-unit2:define-test xref-test ()
  "Test cross references"
  (let* ((raw (get-testing-jso))
	 (xref (gethash "aliases" raw)))
    (assert-true (> (length xref) 0))
    (loop for x in xref
       do (progn
	    (assert-true (> (hash-table-size x) 0))
	    (format t "aliases:~%~c ~{~A~^; ~} ~%" #\Tab
	       (loop for k being the hash-keys in x
		     using (hash-value v)
		      append (list (format nil "~s: ~s" k v)) into records
		      finally (return records)
		 ))
	 )
	 )))




;; Additional utils functions 

(defun get-jso-str (name)
  "Return JDO record identified by NAME."
  (let* ((base-url (format nil "http://www.yeastgenome.org/search?query=~a" name))
	 (page (drakma:http-request base-url))
	 (json-string (bio::--get-bootstrap-json-as-string page))
	 (json-object (st-json:read-json-from-string json-string)))
    json-object))


(defun get-testing-jso ()
  (get-sgd "YPL252C"))
