;; Collection of different utilities 

(in-package #:bio-utils)

(require 'split-sequence)
(require 'ironclad)

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


(defun generate-sha1 ()
  "Generate random and unique SHA1 number."
  (setq *random-state*
	#+sbcl (sb-ext:seed-random-state t)
	#-sbcl (make-random-state t)
	)
  (let* ((salt-string "That is%%^ string which will be initial seed for all work")
	 (current-date-time (write-to-string (get-universal-time)))
	 (random-number (subseq (write-to-string (random 1.0d0)) 2 15))
	 (random-string (concatenate 'string  random-number salt-string current-date-time)))
    (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array random-string)))))
