;; ArrayExpress parsers
(in-package #:bio-utils)

(require 'cl-libxml2)
(require 'drakma)


;; some utilities
(defun value-single-node (obj xpath)
  (libxml2.tree:text-content (libxml2.xpath:find-single-node obj xpath)))


(defun puthash (attr value ht)
  (if (not (hash-table-p ht)) (error "the last parameter is not a hash-table" ))
  (setf (gethash attr ht) value))

;;;; END


(defun --sampleattribute-nodes-parser (sampleattribute-nodes)
  "Returns sampleattribute list"
  (let ((to-return (make-hash-table)))
    (loop for node in sampleattribute-nodes
       for category = (value-single-node node "category")
       for value-nodes = (libxml2.xpath:find-list node "value")
       for values = (loop for v in value-nodes
			    collect (libxml2.tree:text-content v))
	  do (progn 
	       (puthash 'category category to-return)
	       (puthash 'values values to-return)))
    to-return))


(defun --experimental-factor-parser (experimental-factor-node)
  "Parse experimental factor description from EXPERIMENAL-FACTOR-NODE."
  (let ((to-return (make-hash-table))
	(name (value-single-node experimental-factor-node "name"))
	(values (loop for v in (libxml2.xpath:find-list experimental-factor-node "value")
			   collect (libxml2.tree:text-content v))))
    (puthash 'name name to-return)
    (puthash 'values values to-return)
    to-return))

(defun --bibliography-parser (experiment-node)
  "Parse bibliography record from EXPERIMENAL-FACTOR-NODE."
  (let* ((to-return (make-hash-table))
	 (bibliography-node (libxml2.xpath:find-single-node experiment-node "bibliography"))
	 (accession (value-single-node bibliography-node "accession"))
	 (authors (value-single-node bibliography-node "authors"))
	 (title (value-single-node bibliography-node "title"))
	 (issue (value-single-node bibliography-node "issue"))
	 (pages (value-single-node bibliography-node "pages"))
	 (publication (value-single-node bibliography-node "publication"))
	 (volume (value-single-node bibliography-node "volume"))
	 (year (value-single-node bibliography-node "year")) 
    (puthash 'accession accession to-return)
    (puthash 'authors authors to-return)
    (puthash 'title title to-return)
    (puthash 'issue issue to-return)
    (puthash 'pages pages to-return)
    (puthash 'publication publication to-return)
    (puthash 'volume volume to-return)
    (puthash 'year year to-return)
    to-return
    )))


;; fixed main method
(defun call-experiment-array-express (accession)
  "Returns experiment data identified by ACCESSION as a hashmap from ArrayExpress."
  (let* ((output-string (get-array-express accession 'experiments))
	 (page (libxml2.tree:parse output-string))
	 (to-return (make-hash-table))
	 (experiments (libxml2.xpath:find-single-node page "//experiments"))
        ;;	 (experiments-attrs (load-experiments-attributes experiments)) ;; This line's result is not used
	 (experiment-node (libxml2.xpath:find-single-node experiments "experiment"))
	 (experimental-factor-node (libxml2.xpath:find-single-node experiment-node "experimentalfactor"))
	 (description-text (value-single-node experiment-node "description/text"))
	 (sampleattribute-nodes (libxml2.xpath:find-list experiment-node "sampleattribute"))
	 (name (value-single-node experiment-node "name")))
    (format *error-output* "name: ~S~%" name)
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
				   (tot-assays "total-assays")) experiments-node
      (acons 'version ver to-return)
      (acons 'revision rev to-return)
      (acons 'total total to-return)
      (acons 'total-sampl tot-sampl to-return)
      (acons 'total-assays tot-assays to-return))
    to-return))


(defun get-array-express (accession domain)
  "Request ArrayExpress database by DOMAIN ('EXPERIMENTS or 'FILES) and return as string."
  (let* ((domain-text (symbol-name domain))
	 (template (cdr (assoc "array-express" *data-sources* :test 'equal)))
	 (final-url (format nil template domain-text accession)))
    (format *standard-output* "final url: ~s~%" final-url)
    (drakma:http-request final-url :method :get :user-agent :firefox)))


