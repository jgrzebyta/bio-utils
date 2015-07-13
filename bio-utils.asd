;;;; bio-utils.asd

(asdf:defsystem #:bio-utils
  :description "Data integrity tools for biology domain."
  :author "Jacek Grzebyta <jgrzebyta@users.noreply.github.com>"
  :license "GPLv3"
  :depends-on (#:drakma
               #:cl-html5-parser
               #:cl-ppcre
               #:st-json)
  :serial t
  :components ((:file "package")
               (:file "sgd-parser")))

(asdf:defsystem #:bio-utils-test
  :depends-on (#:bio-utils #:lisp-unit2)
  :serial t
  :components ((:module "test"
			:components ((:file "sgd-test")))))
