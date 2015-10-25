;; ArrayExpress parsers
(in-package #:bio-utils)

(require 'cl-libxml2)
(require 'drakma)

(defun --sampleattribute-nodes-parser (sampleattribute-nodes)
  "Returns sampleattribute list"
  (let ((to-return (make-hash-table :test 'equal)))
    (loop for node in sampleattribute-nodes
	  for category = (first (xml-node-children (car (xml-get-children node 'category))))
	  for value-nodes = (xml-get-children node 'value)
	  for value = (loop for v in value-nodes
			    for v-value = (first (xml-node-children v))
			    collect v-value
			    )
	  do (puthash category value to-return)
	  )
    to-return))


(defun --experimental-factor-parser (experimental-factor-node)
  "Parse experimental factor description from EXPERIMENAL-FACTOR-NODE."
  (let ((to-return (make-hash-table))
	(name (first (xml-node-children (car (xml-get-children experimental-factor-node 'name)))))
	(value-nodes (loop for v in (xml-get-children experimental-factor-node 'value)
			   collect (first (xml-node-children v)))))
    (puthash 'name name to-return)
    (puthash 'values value-nodes to-return)
    to-return
    ))


(defun --bibliography-parser (experimental-factor-node)
  "Parse bibliography record from EXPERIMENAL-FACTOR-NODE."
  (let* ((to-return (make-hash-table))
	 (bibliography-node (car (xml-get-children experimental-factor-node 'bibliography)))
	 (accession (car (xml-node-children (car (xml-get-children bibliography-node 'accession)))))
	 (authors (car (xml-node-children (car (xml-get-children bibliography-node 'authors)))))
	 (title (car (xml-node-children (car (xml-get-children bibliography-node 'title)))))
	 (issue (car (xml-node-children (car (xml-get-children bibliography-node 'issue)))))
	 (pages (car (xml-node-children (car (xml-get-children bibliography-node 'pages)))))
	 (publication (car (xml-node-children (car (xml-get-children bibliography-node 'publication)))))
	 (volume (car (xml-node-children (car (xml-get-children bibliography-node 'volume)))))
	 (year (car (xml-node-children (car (xml-get-children bibliography-node 'year))))))
    (puthash 'accession accession to-return)
    (puthash 'authors authors to-return)
    (puthash 'title title to-return)
    (puthash 'issue issue to-return)
    (puthash 'pages pages to-return)
    (puthash 'publication publication to-return)
    (puthash 'volume volume to-return)
    (puthash 'year year to-return)
    to-return
    ))


;; fixed main method
(defun call-experiment-array-express (accession)
  "Returns experiment data identified by ACCESSION as a hashmap from ArrayExpress."
  (let* ((output-string (get-array-express accession 'experiments))
	 (page (libxml2.tree:parse output-string))
	 (to-return (make-hash-table))
	 (experiments (libxml2.xpath:find-single-node page "//experiments"))
	 (experiments-attrs (load-experiments-attributes experiments))
	 (experiment-node (libxml2.xpath:find-single-node experiments "experiment"))
	 (experimental-factor-node (libxml2.xpath:find-single-node experiment-node "experimentalfactor"))
	 (description-node (libxml2.xpath:find-single-node experiment-node "description"))
	 (description-text (libxml2.tree:text-content (libxml2.xpath:find-single-node description-node "text")))
	 (sampleattribute-nodes (libxml2.xpath:find-list experiment-node "sampleattribute"))
	 (name (libxml2.tree:text-content (libxml2.xpath:find-single-node experiment-node "name"))))
    (message "name: %S" name)
    (puthash 'accession accession to-return)
    (puthash 'name name to-return)
    (puthash 'experimental-factor (--experimental-factor-parser experimental-factor-node) to-return)
    (puthash 'sample-attribute (--sampleattribute-nodes-parser sampleattribute-nodes) to-return)
    (puthash 'description description-text to-return)
    (puthash 'bibliography (--bibliography-parser experiment-node) to-return)
    to-return
    ))

(defun load-experiments-attributes (experiments-node)
  ;; with-attributes example
  (let ((to-return nil))
    (libxml2.tree:with-attributes ((ver "version")
				   (rev "revision")
				   (total "total")
				   (tot-sampl "total-samples")
				   (tot-assays "total-assays")) experiment-nodes
      (acons 'version ver to-return)
      (acons 'revision rev to-return)
      (acons 'total total to-return)
      (acons 'total-sampl to-return)
      (acons 'total-assays to-return))
    to-return))


(defun get-array-express (accession domain)
  "Request ArrayExpress database by DOMAIN ('EXPERIMENTS or 'FILES) and return as string."
  (let* ((domain-text (symbol-name domain))
	 (template (cdr (assoc "array-express" *data-sources* :test 'equal)))
	 (final-url (format nil template domain-text accession)))
    (format *standard-output* "final url: ~s~%" final-url)
    (drakma:http-request final-url :method :get :user-agent :firefox)))


