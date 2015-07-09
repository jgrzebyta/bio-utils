(require 'closure-html)
(require 'drakma)

(let* ((id "YBL045C")
       (base-url (format nil "http://www.yeastgenome.org/search?query=~A" id))
       (page (drakma:http-request base-url))
       (page-parsed (chtml:parse page (chtml:make-string-sink))))
  (format t "page:~%~s~%" (type-of page-parsed))
)
