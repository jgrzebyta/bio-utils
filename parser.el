;; Bunch of parsers
(provide 'parser)

(require 'download)
(require 'xml)
(require 'cl)


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
	 (experimental-factor-node (car (xml-get-children experiment-node 'experimentalfactor)))
	 (description-node (car (xml-get-children experiment-node 'description)))
	 (description-text (first (xml-node-children (car (xml-get-children description-node 'text)))))
	 (sampleattribute-nodes (xml-get-children experiment-node 'sampleattribute))
	 (name (first (xml-node-children (car (xml-get-children experiment-node 'name))))))
    (message "name: %S" name)
    (push (cons "accession" accession) to-return )
    (push (cons "name" name) to-return)
    (push (cons "total-experiments" total-attr) to-return)
    (push (cons "total-smaples" total-samples-attr) to-return)
    (push (cons "total-assays" total-assays-attr) to-return)
    (push (cons "experimental-factor" (--experimental-factor-parser experimental-factor-node)) to-return)
    (push (cons "sample-attribute" (--sampleattribute-nodes-parser sampleattribute-nodes)) to-return)
    (push (cons "description" description-text) to-return)
    (push (cons "bibiography" --bibliography-parser) to-return)
    ))


(defun --sampleattribute-nodes-parser (sampleattribute-nodes)
  "Returns sampleattribute list"
  (let ((node-parsed nil))
    (loop for node in sampleattribute-nodes
	  for category = (first (xml-node-children (car (xml-get-children node 'category))))
	  for values = (xml-get-children node 'value)
	  for value = nil
	  do (progn
	      (loop for v in values
		   for v-value = (first (xml-node-children v))
		   do (push v-value value))
	      (reverse value))
	  collect (cons category value)
	  )))


(defun --experimental-factor-parser (experimental-factor-node)
  "Parse experimental factor description from EXPERIMENAL-FACTOR-NODE."
  (let ((name (first (xml-node-children (car (xml-get-children experimental-factor-node 'name)))))
	(value-nodes (loop for v in (xml-get-children experimental-factor-node 'value)
			   collect (first (xml-node-children v)))))
    (concat "\"" name "\"" " with variants: " (mapconcat (lambda (x) (format "%s" x)) value-nodes ", "))))


(defun --bibliography-parser (experimental-factor-node)
  "Parse bibliography record from EXPERIMENAL-FACTOR-NODE."
  (let ((bibliography-node (car (xml-get-children experimental-factor-node 'bibliography)))
	(accession (car (xml-node-children (xml-get-children bibliography-node 'accession))))
	(authors (car (xml-node-children (xml-get-children bibliography-node 'authors))))
	(title (car (xml-node-children (xml-get-children bibliography-node 'title))))
	(issue (car (xml-node-children (xml-get-children bibliography-node 'issue))))
	(pages (car (xml-node-children (xml-get-children bibliography-node 'pages))))
	(publication (car (xml-node-children (xml-get-children bibliography-node 'publication))))
	(volume (car (xml-node-children (xml-get-children bibliography-node 'volume))))
	(year (car (xml-node-children (xml-get-children bibliography-node 'year)))))
    (list (cons "accession" accession)
	  (cons "authors" authors)
	  (cons "title" title)
	  (cons "issue" issue)
	  (cons "pages" pages)
	  (cons "publication" publication)
	  (cons "volume" volume)
	  (cons "year" year))))

