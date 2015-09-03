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


(defun multiline-string-list (is sep)
  "Do the same as STRING-LIST except build list of lists. 

If IS is a multi lines string then in the first instance split it into list of lines. In the second spep parse each line using STRING-LIST funcion.
Skip empty (length <= 0) line."
  (let ((line-splited (split-sequence:split-sequence #\Newline is)))
    (loop for line in line-splited
	 if (> (length line) 0)
         collect (string-list line sep))))


(defun string-remove-citations (is)
  "Remove citation characters #\' and #\\\" from IS string."
  (string-trim '(#\' #\") is))
