
(provide 'parser)

(require 'download)
(require 'xml)



(xml-parse (xml-string object-parser)
"Parse xml from XML-STRING using function OBJECT-PARSER.
That function should accepts .... as input"
(let* ((root (with-temp-buffer
	      (insert xml-string)
	      (xml-parse-region (point-min) (point-max))))
       ())
  ())
  )
