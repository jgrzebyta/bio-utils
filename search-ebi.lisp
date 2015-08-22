(in-package #:bio-utils)

(require 'drakma)
(require 'cl-libxml2)

(
 #-SBCL defconstant
 #+SBCL sb-int:defconstant-eqx
    +data-sources+ '(("uniprot" . "http://www.uniprot.org/uniprot/~a.~a")
		     ("ebi-eye" . "http://www.ebi.ac.uk/ebisearch/ws/rest/~a?query=~a")
		     ("ebi-eye-description" . "http://www.ebi.ac.uk/ebisearch/ws/rest/~a")
		     ("ebi-eye-xref" . "http://www.ebi.ac.uk/ebisearch/ws/rest/~a/entry/~a/xref/") ;; display xref to different databases
		     ("ebi-eye-xref-id" . "http://www.ebi.ac.uk/ebisearch/ws/rest/~a/entry/~a/xref/~a")) ;; display xref with domain specific ID
    #'equal) 


(defun generic-call (uri)
  (map 'string #'code-char (drakma:http-request uri :method :get :user-agent :firefox)))

(defun call-ebi-eye (data-src term)
  "Call EBI eye REST service"
  (let* ((db-pattern (cdr (assoc "ebi-eye" +data-sources+ :test 'equal)))
	 (uri-request (format nil db-pattern data-src term)))
    ;;(format t "uri: ~s~%" uri-request)
    (generic-call uri-request)))


(defun call-ebi-eye-xref (data-src term)
  "Call EBI eye REST service"
  (let* ((db-pattern (cdr (assoc "ebi-eye-xref" +data-sources+ :test 'equal)))
	 (uri-request (format nil db-pattern data-src term)))
    (format t "uri: ~s~%" uri-request)
    (generic-call uri-request)))

(defun call-ebi-eye-xref-parser (in-structure)
  (let ((results nil)
	(xpath-pattern "//domain"))
    (loop for node in (libxml2.xpath:find-list in-structure xpath-pattern)
	 do (progn 
	      (setf results (append results (list (libxml2.tree:attribute-value node "id"))))
	      (format t "result: ~s ~%" results)))
    results))

(defun call-ebi-eye-description (data-src)
  (let* ((db-pattern (cdr (assoc "ebi-eye-description" +data-sources+ :test 'equal)))
	 (uri-request (format nil db-pattern data-src)))
    (format t "uri: ~s ~%" uri-request)
    (generic-call uri-request)))


;; (defun call-uniprot (term)
;;   (let* ((db-pattern (cdr (assoc "uniprot" +data-sources+ :test 'equal)))
;; 	 (uri-request (format nil db-pattern term "xml"))))
;;   (format t "uri: ~s~%" uri-request)
;;   (generic-call uri-request))

;; Parser XML file output
(defun count-ebi-eye-parser (in-structure)
  "Parser for allebi datasource request"
  (let ((results (make-hash-table :test 'equal))
	(xpath-pattern "//domain[not(subdomains)]"))
    (loop for node in (libxml2.xpath:find-list in-structure xpath-pattern)
       do (let ((id nil)
		(hits 0))
	    (setq id (libxml2.tree:attribute-value node "id"))
	    (setq hits (first (multiple-value-list (parse-integer (libxml2.tree:text-content (libxml2.tree:first-child node))))))
	    (format t "   database: ~s    hits: ~s  ~% " id hits)
	    (if (not (= hits 0))
		(setf (gethash id results) hits))
	    ))
    results)
  )

(defun targets-ebi-eye-parser (in-structure)
  "Parser for particular dataset"
  (let ((results (make-hash-table :test 'equal))
	(xpath-pattern "//entry"))
    (loop for node in (libxml2.xpath:find-list in-structure xpath-pattern)
       do (let ((source (libxml2.tree:attribute-value node "source"))
		(acc (libxml2.tree:attribute-value node "acc"))
		(id (libxml2.tree:attribute-value node "id")))
	    ;;(format t "source: ~sid: ~s~%" source id)
	    (setf (gethash source results) (cons acc id)))
	 )
    results)
  )


(defun test-call-ebi-eye ()
  (format t "result: ~a~%" (targets-ebi-eye-parser (libxml2.tree:parse (call-ebi-eye "uniprot" "cgd4_2720")))))

;;(test-call-ebi-eye)
;;(format t "~%~A~%" (call-ebi-eye "uniprot" "cgd4_2720"))

(defun main ()
  (loop for line = (read-line *standard-input* nil)
     while line
     do (let* ((in (libxml2.tree:parse (call-ebi-eye "uniprot" line)))
	       (results (targets-ebi-eye-parser in)))
	  (if (> (hash-table-count results) 0)
	      (loop for key being the hash-keys in results
		    using (hash-value value)
		 do (format t "~a~C~a~C~a~%" line #\tab key #\tab (car value)))
	      (format t "~a~C~a~C~a~%" line #\tab nil #\tab nil))
	  )))


(main)



;; (defun main2 ()
;;   (loop for line = (read-line *standard-input* nil)
;;      while line
;;      do (let* ((in (libxml2.tree:parse (call-ebi-eye-xref "uniprot" line)))
;; 	       (results (call-ebi-eye-xref-parser in)))
;; 	  ;;(format t "~a~C~a~%") line #\tab results)
;; 	  ))

;; (main2)

;; (defun uniprot-targets ()
;;   (loop for line = (read-line *standard-input* nil)
;;      while line
;;      do (let* ((in (libxml2.tree:parse (call-ebi-eye "uniprot" line)))
;; 	       (results (targets-ebi-eye-parser in)))
;; 	  (loop for k being the hash-keys in results
;; 	     using (hash-value v)
;; 	     do (format t "~a~C~a~C~a~%" k #\tab (car v) #\tab (cdr v))  ))
;;        ))


;; (uniprot-targets)

