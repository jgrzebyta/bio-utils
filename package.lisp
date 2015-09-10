;;;; package.lisp

(defpackage #:bio-utils
  (:nicknames #:bio)
  (:use #:cl)
  (:export :*data-sources*
           :get-sgd
	   :string-list
	   :multiline-string-list
	   :generate-sha1))

