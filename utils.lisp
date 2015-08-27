;; Collection of different utilities 

(in-package #:bio-utils)

(require 'split-sequence)

(defun string-list (is sep)
  "Convert IS string into list. Input string is splited by SEP or #\Space if NIL.
Final text is trimed with #\Tab and #\Space."
  (let* ((separator (or sep #\Space))
	 (list-split (split-sequence:split-sequence separator is)))
    (loop for i in list-split
       collect (string-trim '(#\Tab #\Space) i))))


