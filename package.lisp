;;;; package.lisp

(defpackage #:bio-utils
  (:nicknames #:bio)
  (:use #:cl #:st-json)
  (:export :get-sgd
	   :string-list
	   :multiline-string-list))

