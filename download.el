;; Download sequences
(provide 'download)


(defconst *data-sources* '(("uniprot" . "http://www.uniprot.org/uniprot/%s.%s")
			   ("ebi-eye" . "http://www.ebi.ac.uk/ebisearch/ws/rest/%s?query=%s")
			   ("array-express" . "http://www.ebi.ac.uk/arrayexpress/xml/v2/%s?accession=%s")))



(defvar http-result-processing 'remove-header-test)


(defun remove-header-to-buffer (status) 
  "Delete HTTP header"
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (when (string-match "^.* 200 OK$" (thing-at-point 'line))
    (search-forward "\n\n")
    (delete-region (point-min) (point)))
    (switch-to-buffer (current-buffer))))

(defun remove-header-to-string (status)
   (with-temp-buffer
    (goto-char (point-min))
    (when (string-match "^.* 200 OK$" (thing-at-point 'line))
      (search-forward "\n\n")
      (delete-region (point-min) (point)))
    (message "test output: '%s'" (buffer-name (current-buffer)))
    (buffer-string)))

(defun remove-header-test (status)
  (with-temp-buffer
    (message "test out: %s" (buffer-name (current-buffer)))
    (insert "some very message")
    (buffer-string)))


(defun call-generic (url)
  "Download from url"
  (let ((url-request-method "GET"))
    (message "http-result-processing: %s" http-result-processing)
    (url-retrieve url http-result-processing)))


(defun call-ebi-eye (term domain)
  (let* ((template (cdr (assoc "ebi-eye" *data-sources*)))
	 (final-url (format template domain term)))
    (message "final url: %s" final-url)
    (call-generic final-url)))


(defun call-array-express (accession domain)
  "Request ArrayExpress database by DOMAIN ('EXPERIMENTS or 'FILES)"
  (let* ((domain-text (cond ((eq domain 'experiments) "experiments")
			    ((eq domain 'files) "files")
			    (t (error "no other domain  is accepted"))))
	 (template (cdr (assoc "array-express" *data-sources*)))
	 (final-url (format template domain-text accession)))
    (message "final url: %s" final-url)
    (call-generic final-url)))
