;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
(in-package #:bio-utils)

(defparameter *data-sources* '(("uniprot" . "http://www.uniprot.org/uniprot/~a.~a")
			       ("ebi-eye" . "http://www.ebi.ac.uk/ebisearch/ws/rest/~a?query=~a")
			       ("array-express" . "http://www.ebi.ac.uk/arrayexpress/xml/v2/~a?accession=~a")
			       ("yeast-genome-search" . "http://www.yeastgenome.org/search?query=~a"))
  "This parameter contains an alist of all required datasources. In the future the content might be loaded more dinamicaly by a module.")
