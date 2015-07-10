
(require 'closure-html)
(require 'drakma)
(require 'cxml-stp) ;; DOM parser, features rich

(defparameter content-class "content-column")
(defparameter content-element "div")


(let* ((id "ybl045c")
       (base-url (format nil "http://www.yeastgenome.org/search?query=~a" id))
       (page (drakma:http-request base-url))
       (page-parsed (chtml:parse page (stp:make-builder))))
  (format t "url request: ~s~%" base-url)
  (format t "output-type: ~s~%" (type-of page-parsed))
  (stp:do-recursively (div page-parsed)
    ;;(format t "div item: ~S~%" div)
    (format t "is element: ~s~%" (typep div 'stp:element))
    (format t "has class: ~s~%" (equal (stp:local-name div) content-element))
    (when (and (typep div 'stp:element)
	       (equal (stp:local-name div) content-element)
	       ;;(search content-class (stp:attribute-value div content-element))
	       )
      (format t "element: ~S~%" (stp:attribute-value div "class")))
    )
)


