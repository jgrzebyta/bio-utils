;; Download sequences
(provide 'download)


(defconst *data-sources* '(("uniprot" . "http://www.uniprot.org/uniprot/%s.%s")
			   ("ebi-eye" . "http://www.ebi.ac.uk/ebisearch/ws/rest/%s?query=%s")))



(defvar http-result-processing 'remove-header)


(defun remove-header (status) 
  "Delete HTTP header"
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (when (string-match "^.* 200 OK$" (thing-at-point 'line))
    (search-forward "\n\n")
    (delete-region (point-min) (point)))
    (switch-to-buffer (current-buffer))))

(defun call-generic (url)
  "Download from url"
  (let ((url-request-method "GET"))
    (url-retrieve url 'http-result-processing)))


(defun call-ebi-eye (term domain)
  (let* ((template (assoc "ebi-eye" *data-sources*))
	 (final-url (format template domain term)))
    (message "final url: %s" final-url)
    (call-generic final-url)))
