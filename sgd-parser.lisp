

;; Saccharomyces Genome Database web page parser
;; This parser collects information from yeast strain web page and converts it into useful hashmap.
;;
;; Useful for QuickLoad for manual launching 
;; (ql:quickload 'drakma)
;; (ql:quickload 'cl-html5-parser)
;; (ql:quickload 'cl-ppcre)
;; (ql:quickload 'st-json)

(require 'drakma)
(require 'cl-html5-parser)
(require 'cl-ppcre)
(require 'st-json)



(defun -get-bootstrap-json-as-string (in-string)
  "Extract value of bootstappedData from IN-STRING and return as string."
  (let* ((parsed (html5-parser:parse-html5 in-string :dom :xmls))
	 (head (nth 3 parsed))
	 (script (nth 3 head))
	 (code (ppcre:regex-replace-all "\\r?\\n?" (nth 2 script) ""))
	 (to-return nil))
    (multiple-value-bind (result groups) (ppcre:scan-to-strings "var bootstrappedData = (\{.+\});" code)
      (format t "groups type: ~a~%" (type-of groups))
      (elt groups 0)  ;; unpack data from vector
      )))

(defun -get-json-as-hashmap (json-object)
  "Converts internal type of JSO instance JSON-OBJECT into custom hashmap"
  
  )

(defun get-sgd (name)
  "Return Saccharomyces Genome Database record identified by NAME as a hashmap."
  (let* ((base-url (format nil "http://www.yeastgenome.org/search?query=~a" name))
	 (page (drakma:http-request base-url))
	 (json-string (-get-bootstrap-json-as-string page))
	 (json-object (st-json:read-json-from-string json-string)))
    (-get-json-as-hashmap json-object)))


