(require 'closure-html)
(require 'drakma)
(require 'cxml-stp) ;; DOM parser, features rich

(defconstant *content-class* "content-column")
(defconstant *content-element* "div")


(let* ((id "YBL045C")
       (base-url (format nil "http://www.yeastgenome.org/search?query=~A" id))
       (page (drakma:http-request base-url))
       (page-parsed (chtml:parse page (stp:make-builder))))
  (format t "URL request: ~s~%" base-url)
  (format t "output-type: ~s~%" (type-of page-parsed))
)
