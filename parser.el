;; Bunch of parsers
(provide 'parser)

(require 'download)
(require 'xml)


(defun get-experiment-array-express (accession)
  "Returns experiment data identified by ACCESSION as a hashmap from ArrayExpress."
  (let* ((output (call-array-express accession 'experiments))
	 (root (with-temp-buffer
		 (insert output)
		 (xml-parse-region (point-min) (point-max))))
	 (to-return nil)
	 (experiments (car root))
	 (experiments-attrs (xml-node-attributes experiments))
	 (total-attr (cdr (assq 'total experiments-attrs)))
	 (total-samples-attr (cdr (assq 'total-samples experiments-attrs)))
	 (total-assays-attr (cdr (assq 'total-assays experiments-attrs)))
	 (experiment-node (car (xml-get-children experiments 'experiment)))
	 (description-node (car (xml-get-children experiment-node 'description)))
	 (description-text (first (xml-node-children (car (xml-get-children description-node 'text)))))
	 (sampleattribute-node (xml-get-children experiment-node 'sampleattribute)))
    (push (cons "accession" accession) to-return )
    (push (cons "total-experiments" total-attr) to-return)
    (push (cons "total-smaples" total-samples-attr) to-return)
    (push (cons "total-assays" total-assays-attr) to-return)
    (push (cons "description" description-text) to-return)
    ))


(defun --sampleattribute-nodes-parser (sampleattribute-nodes)
  "Returns sampleattribute list"
  (let ((node-parsed nil))
    (loop for node in sampleattribute-nodes
	  for category = (first (xml-node-children (car (xml-get-children node 'category))))
	  for value = (xml-get-children node 'value)
	  do (message "type: %s   " (type-of category))
	  collect (cons category value)
	  )))
