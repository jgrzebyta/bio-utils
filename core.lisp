;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;; Metadata class for a parser.
(defclass parser ()
  (:documentation "Basic class for all parsers. Instance of the class handles parser matadata. The main parsing job is done by methods.")
  ((name :initarg :name :accessor name
	 :documentation "This parser name")
   (xref-name :initarg :xref-name :accessor xref-name
	      :documentation "Parser's target name - 1 word")
   (xref-ns :initarg :xref-ns :accessor xref-ns
		   :documentation "Parser's target namespace as URI")))

(defgeneric parse-stream (parser is)
  (:documentation "PARSER do the parsing action from IS input-stream."))

(defgeneric parse-string (parser string)
  (:documentation "PARSER does the parsing action from STRING."))
