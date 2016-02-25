;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
(in-package #:bio-utils)

(defparameter *data-sources* '(("uniprot" . "http://www.uniprot.org/uniprot/~a.~a")
			       ("uniprot-search-tab" . "http://www.uniprot.org/uniprot/?search=~a&format=tab&columns=id,entry+name,reviewed,protein+names,genes,organism,organism-id")
			       ("uniprot-search-rdf" . "http://www.uniprot.org/uniprot/?search=~a&format=rdf")
			       ("ebi-eye" . "http://www.ebi.ac.uk/ebisearch/ws/rest/~a?query=~a")
			       ("array-express" . "http://www.ebi.ac.uk/arrayexpress/xml/v2/~a?accession=~a")
			       ("yeast-genome-search" . "http://www.yeastgenome.org/search?query=~a")
			       ("yeastract-download" . "http://www.yeastract.com/download/~a"))
  "This parameter contains an alist of all required datasources. In the future the content might be loaded more dinamicaly by a module.")
