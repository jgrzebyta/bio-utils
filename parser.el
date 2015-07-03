;; Bunch of parsers
(provide 'parser)

(require 'download)
(require 'xml)
(require 'cl)





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



(defun get-experiment-array-express (accession)
  "Returns experiment data identified by ACCESSION as a hashmap from ArrayExpress."
  (let* ((output (call-array-express accession 'experiments))
	 (root (with-temp-buffer
		 (insert output)
		 (xml-parse-region (point-min) (point-max))))
	 (to-return (make-hash-table))
	 (experiments (car root))
	 (experiments-attrs (xml-node-attributes experiments))
	 (total-attr (cdr (assq 'total experiments-attrs)))
	 (total-samples-attr (cdr (assq 'total-samples experiments-attrs)))
	 (total-assays-attr (cdr (assq 'total-assays experiments-attrs)))
	 (experiment-node (car (xml-get-children experiments 'experiment)))
	 (experimental-factor-node (car (xml-get-children experiment-node 'experimentalfactor)))
	 (description-node (car (xml-get-children experiment-node 'description)))
	 (description-text (first (xml-node-children (car (xml-get-children description-node 'text)))))
	 (sampleattribute-nodes (xml-get-children experiment-node 'sampleattribute))
	 (name (first (xml-node-children (car (xml-get-children experiment-node 'name))))))
    (message "name: %S" name)
    (puthash 'accession accession to-return )
    (puthash 'name name to-return)
    (puthash 'total-experiments total-attr to-return)
    (puthash 'total-smaples total-samples-attr to-return)
    (puthash 'total-assays total-assays-attr to-return)
    (puthash 'experimental-factor (--experimental-factor-parser experimental-factor-node) to-return)
    (puthash 'sample-attribute (--sampleattribute-nodes-parser sampleattribute-nodes) to-return)
    (puthash 'description description-text to-return)
    (puthash 'bibliography (--bibliography-parser experiment-node) to-return)
    to-return
    ))
