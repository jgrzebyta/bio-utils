(in-package #:bio-utils)

(require 'cl-ppcre)


(defun string-has-text-p (string)
  "Predicate function to check if requested string has any non white characters."
  (let ((string-length (length (string-trim '(#\Space #\Tab #\NewLine) string))))
    (if (= 0 string-length) nil t)))


(defun array-to-list (a)
  (map 'list (lambda (x) x) a))

(defun parse-input ()
  (let ((records (make-hash-table :test 'equal))
	(order-queue nil)
	(target-id nil))
	(loop for line = (read-line *standard-input* nil)
	      while line
              do (let ((record-scan (multiple-value-list (cl-ppcre:scan-to-strings "\s*\>(.*)" line))))
		   (cond
		     ((not (null (car record-scan))) (if (null (member (car record-scan) order-queue :test 'equal))  ;;;; parse protein ID
							 (progn
							   (setf target-id (first record-scan))
							   (setf order-queue (append order-queue (list target-id))))
							   (setf target-id nil))
		                                  ;;   (format t "id: ~s~%" target-id)
		      )
		     ((not (null target-id)) ;;;; parse protein sequence
		      (if (string-has-text-p line)
			  (setf (gethash target-id records) (concatenate 'string  
							        (gethash target-id records) line))
			  )
					;;	(format t "id: ~s    sequence: ~s~%" target-id (gethash target-id records))
			)
		     )))
	(values-list (list order-queue records))))


(defun recover-fasta (data &key is-flat output-stream has-extraline)
"Recreate a fasta file from input data.
recover-fasta (data &key is-flat output-stream) => NIL

If is-flat is t than display everything in 1 line in following format: protein-id\#tabsequence.
If output-stream is not given or is null than use *standard-output*.
If has-extraline is true than extra empty line is added after each sequence.
"
(setf output-stream (or output-stream *standard-output*))
(let ((separator-char (if is-flat #\tab #\NewLine)))
  (loop for prot-id being the hash-keys of data
       using (hash-value seq)
       ;;when (not (null prot-id))  ;; remove a case when protein-id is null
       do (progn
	   (format output-stream "~a~C~a~C" prot-id separator-char seq #\NewLine)
	   (if has-extraline (format output-stream "~C" #\NewLine))))
  )
)

;; TODO: move that method to test
;; (defun main ()
;;   (multiple-value-bind (queue data) (parse-input)
;;     (recover-fasta data :is-flat nil))
;;   )

;; (main)
