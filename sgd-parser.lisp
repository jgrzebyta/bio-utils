
 
;; Saccharomyces Genome Database web page parser
;; This parser collects information from yeast strain web page and converts it into useful hashmap.
;;
;; Useful for QuickLoad for manual launching 
;; (ql:quickload 'drakma)
;; (ql:quickload 'cl-html5-parser)
;; (ql:quickload 'cl-ppcre)
;; (ql:quickload 'st-json)


(in-package #:bio-utils)


(require 'drakma)
(require 'cl-html5-parser)
(require 'cl-ppcre)
(require 'st-json)

(defun --get-bootstrap-json-as-string (in-string)
  "Extract value of bootstappedData from IN-STRING and return as string."
  (let* ((parsed (html5-parser:parse-html5 in-string :dom :xmls))
	 (head (nth 3 parsed))
	 (script (nth 3 head))
	 (code (ppcre:regex-replace-all "\\r?\\n?" (nth 2 script) "")))
    (multiple-value-bind (result groups) (ppcre:scan-to-strings "var bootstrappedData = (\{.+\});" code)
      (format nil "~s" result)
      (elt groups 0)  ;; unpack data from vector
      )))

(defun --load-aliases (json-objects)
  "Load 'locusData.qualities.aliases' field from json-object and load into return-hash."
	 (loop for item in json-objects
	       for to-return = (make-hash-table :test #'equal)
	    do (progn
		 (setf (gethash "category" to-return) (getjso "category" item))
		 (setf (gethash "display-name" to-return) (getjso "display_name" item))
		 (setf (gethash "source" to-return) (getjso* "source.display_name" item))
		 (setf (gethash "link" to-return) (getjso "link" item)))
	    collect to-return))

(defun --load-go (json-objects)
  "Load Gene Ontology terms"
  (let ((to-return (make-hash-table :test #'equal)))
    (loop for term in '("molecular_function" "biological_process" "cellular_component")
	  for term-nodes = nil
	  for response-nodes = nil
       do (progn
	    (setq term-nodes (getjso term json-objects))
	    (setq response-nodes
		  (loop for node in term-nodes
		     for response = nil
		        do (setq response (--load-go-atom node))
		     collect response))
	    (setf (gethash term to-return) response-nodes))) ;; load return hash-table
    to-return ;; return final result
    )
  )

(defun --load-go2 (jso-go-terms)
  "Load Gene Ontology terms. Rewritten version of --load-go method"
  ;; Following return hash table has layout: [ <term> : (( go_id . go_term) ...) ]
  (let ((to-return (make-hash-table :test #'equal)))
    (loop for term in '("molecular_function" "biological_process" "cellular_component")
          for term-value = (gethash term to-return)
       do (mapjso #'(lambda (k v) (if (search term k)
				      ()
				      ))
		  jso-go-terms)) ;; load return hash-table
    to-return ;; return final result
    )
  
  )


(defun --load-go-atom (go-atom)
  "Load Gene Ontology single atom"
  (let ((format-name (getjso* "term.format_name" go-atom)))
    (if (search "GO:" format-name)
	(cons (getjso* "term.display_name" go-atom) format-name)
	nil)))

(defun --get-json-as-hashmap (json-object)
  "converts internal type of jso instance json-object into custom hashmap"
  (let ((to-return (make-hash-table :test #'equal))) ;; probably the block of setf-s bellow might be shorter.
    (setf (gethash "display-name" to-return) (getjso "displayName" json-object))
    (setf (gethash "format-name" to-return) (getjso "formatName" json-object))
    (setf (gethash "locus-id" to-return) (getjso "locusId" json-object))
    (setf (gethash "locus-link" to-return) (getjso "locusLink" json-object))
    (setf (gethash "uniprot-id" to-return) (getjso* "locusData.uniprotid" json-object))
    (setf (gethash "aliases" to-return) (--load-aliases (getjso* "locusData.aliases" json-object)))
    (setf (gethash "go-terms" to-return) (--load-go2 (getjso* "locusData.go_overview" json-object)))
	to-return
    )
)

(defun get-sgd (name)
  "Return Saccharomyces Genome Database record identified by NAME as a hashmap."
  (let* ((base-url (format nil "http://www.yeastgenome.org/search?query=~a" name))
	 (page (drakma:http-request base-url))
	 (json-string (--get-bootstrap-json-as-string page))
	 (json-object (st-json:read-json-from-string json-string)))
    (--get-json-as-hashmap json-object)))


