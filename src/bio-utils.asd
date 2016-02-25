;;;; bio-utils.asd

(asdf:defsystem #:bio-utils
  :description "Data integrity tools for biology domain."
  :author "Jacek Grzebyta <jgrzebyta@users.noreply.github.com>"
  :license "GPLv3"
  :depends-on (#:drakma
               #:cl-html5-parser
	       #:cl-libxml2
               #:cl-ppcre
               #:st-json
	       #:split-sequence
	       #:ironclad)
  :serial t
  :components ((:file "package")
	       (:file "core")
	       (:file "constants")
	       (:file "utils")
               (:file "sgd-parser")
	       (:file "ae-parser")
	       (:file "search-ebi")
	       (:file "normalise-fasta")
	       (:file "yst-parser")))

(asdf:defsystem #:bio-utils-test
  :depends-on (#:bio-utils #:lisp-unit2)
  :serial t
  :components ((:module "test"
			:components ((:file "package")
				     (:file "sgd-test")
				     (:file "utils-test")))))
