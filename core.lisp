;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;; Metadata class for a parser.
(defclass parser ()
  (:documentation "Basic class for all parsers. Instance of the class handles parser matadata. The main parsing job is done by methods.")
  ((name :accessor name :documentation "This parser name")
   (xref-name :accessor xref-name :documentation "Parser's target name - 1 word")
   (url :accessor url :documentation "This is functional URL pattern for parser. Eg. Uniprot RDF might be 'http://www.uniprot.org/uniprot/~a.rdf'. ")
   (xref-ns :accessor xref-ns :documentation "Parser's target namespace as URI")))

(defgeneric request (parser request)
  (:documentation "Requests PARSER using REQUEST string. By default that is the only method used in an application."))

;; probably not necessary
(defgeneric parse-stream (parser is)
  (:documentation "PARSER do the parsing action from IS input-stream."))

;; main parsing engine
(defgeneric parse-string (parser string)
  (:documentation "PARSER does the parsing action from STRING."))
