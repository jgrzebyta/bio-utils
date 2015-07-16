
 
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
  "Extracts value of bootstappedData from IN-STRING and return as string."
  (let* ((parsed (html5-parser:parse-html5 in-string :dom :xmls))
	 (head (nth 3 parsed))
	 (script (nth 3 head))
	 (code (ppcre:regex-replace-all "\\r?\\n?" (nth 2 script) "")))
    (multiple-value-bind (result groups) (ppcre:scan-to-strings "var bootstrappedData = (\{.+\});" code)
      (format nil "~s" result)
      (elt groups 0)  ;; unpack data from vector
      )))

(defun --load-aliases (json-objects)
  "Loads 'locusData.qualities.aliases' field from json-object and load into return-hash."
	 (loop for item in json-objects
	       for to-return = (make-hash-table :test #'equal)
	    do (progn
		 (setf (gethash "category" to-return) (getjso "category" item))
		 (setf (gethash "display-name" to-return) (getjso "display_name" item))
		 (setf (gethash "source" to-return) (getjso* "source.display_name" item))
		 (setf (gethash "link" to-return) (getjso "link" item)))
	    collect to-return))

(defun --load-go2 (jso-go-terms)
  "Loads Gene Ontology terms. Rewritten version of --load-go method"
  ;; Following return hash table has layout: [ <term> : (( go_id . go_term) ...) ]
  (let ((to-return (make-hash-table :test #'equal)))
    (progn
      (loop for term in '("molecular_function" "biological_process" "cellular_component")
            for term-value = (gethash term to-return)
                do (mapjso #'(lambda (k v) (if (search term k) ;; value v i a list of atomic go term
					(loop for atomic in v
					    for atomic-parsed = (--load-go-atom atomic)
					   append (if (not (null atomic-parsed))(list atomic-parsed) nil) into atomics
					   finally (progn
						     (setq term-value (append atomics term-value))
						     (setq term-value (remove-duplicates term-value :test #'equal :key #'(lambda (x) (gethash "format-name" x)))))
					    )
				      ))
		  jso-go-terms)
	 (setf (gethash term to-return) term-value))) ;; load return hash-table
   ;; (format t "output: ~a ~%" to-return)
    to-return ;; return final result
    ))


(defun --load-go-atom (go-atom)
  "Loads Gene Ontology single atom"
  (let ((to-return (make-hash-table :test #'equal))
	(format-name (getjso* "term.format_name" go-atom)))
    (if (search "GO:" format-name)
	(progn
	  (setf (gethash "format-name" to-return) format-name)  ;; encodes GO:xxxxx value
	  (setf (gethash "display-name" to-return) (getjso* "term.display_name" go-atom))
	  (setf (gethash "qualifiers" to-return) (car (getjso "qualifiers" go-atom))))
	)
    to-return))

(defun --load-phenotype (jso-phenotype)
  "Exports phenotype from JSO instance. Returns a hash-table."
  (let ((to-return (make-hash-table :test #'equal))) ;; a hash table with experiment types as keys
    (mapjso #'(lambda (exp-k exp-v)
		(let ((predicates-hash-table (make-hash-table :test #'equal))) ;; a hash table with predicates as keys
		    (if (search "_phenotypes" exp-k) ;; for each of experiment types ...
			;; ... read predicates
			(progn
			  (mapjso #'(lambda (pred-k pred-v)
				      (setf (gethash pred-k predicates-hash-table)
					    (loop for phenotype in pred-v
					       for phe-atom-table = (--load-phenotype-atom phenotype)
					       collect phe-atom-table))
				      ) exp-v)
			  (setf (gethash exp-k to-return) predicates-hash-table)))
		    
		))
	    jso-phenotype)
    to-return))


(defun --load-phenotype-atom (jso-phenotype)
  "Converts JSO phenotype atom into hash-table representation"
  (let ((to-return (make-hash-table :test #'equal)))
    (setf (gethash "display-name" to-return) (getjso "display_name" jso-phenotype))
    (setf (gethash "link" to-return) (getjso "link" jso-phenotype))
    (setf (gethash "format-name" to-return) (getjso "format_name" jso-phenotype))
    to-return
    )
  )


(defun --get-json-as-hashmap (json-object)
  "Converts internal type of jso instance json-object into custom hashmap"
  (let ((to-return (make-hash-table :test #'equal))) ;; probably the block of setf-s bellow might be shorter.
    (setf (gethash "display-name" to-return) (getjso "displayName" json-object))
    (setf (gethash "format-name" to-return) (getjso "formatName" json-object))
    (setf (gethash "locus-id" to-return) (getjso "locusId" json-object))
    (setf (gethash "locus-link" to-return) (getjso "locusLink" json-object))
    (setf (gethash "description" to-return) (getjso* "locusData.qualities.description.value" json-object))
    (setf (gethash "uniprot-id" to-return) (getjso* "locusData.uniprotid" json-object))
    (setf (gethash "aliases" to-return) (--load-aliases (getjso* "locusData.aliases" json-object)))
    (setf (gethash "go-terms" to-return) (--load-go2 (getjso* "locusData.go_overview" json-object)))
    (setf (gethash "phenotype" to-return) (--load-phenotype (getjso* "locusData.phenotype_overview" json-object)))
	to-return
    )
)

(defun get-sgd (name)
  "Returns Saccharomyces Genome Database record identified by NAME as a hash-table."
  (let* ((base-url (format nil "http://www.yeastgenome.org/search?query=~a" name))
	 (page (drakma:http-request base-url))
	 (json-string (handler-case
			  (--get-bootstrap-json-as-string page)
			(error () (return-from get-sgd nil))))
	 (json-object (st-json:read-json-from-string json-string)))
    (--get-json-as-hashmap json-object)))


