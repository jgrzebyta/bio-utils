;; ERT tests
(provide 'test)

(require 'f)
(require 'ert)

(defconst test-source-path
  (f-parent (f-this-file)))

(defconst source-path
  (f-parent test-source-path))

;; simplest sollution
(require 'download (f-expand "download.el" source-path))
(require 'parser (f-expand "parser.el" source-path))

(defun get-experiment ()
  "Get E-MEXP-130 experiment record"
  (get-experiment-array-express "E-MEXP-130"))


;; test initialising taken from article http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html
;; not yet


(ert-deftest test-get-nodes ()
  "Take nodes from experiment 'E-MEXP-130'"
  (message "^^^^^^^^^^^^^^^^^^^^^^")
  (let ((experiment (get-experiment)))
    (message "experiment node: %S\n" experiment)
    (should (hash-table-p experiment))))


(ert-deftest test-sampleattributes ()
  "Test parsing of sampleattributes nodes"
  (let* ((experiment (get-experiment))
	(sattr (gethash 'sample-attribute experiment)))
    (message "sample-attribute node: %S\n" sattr)
    (should (hash-table-p experiment))
    (should (member "StrainOrLine" (loop for v being hash-keys of sattr collect v))))
  )

(ert-deftest test-bibliography ()
  "Test parsing bibliography"
  (message "test-bibliography &&&&&&&&&&&&&&&&&&&\n")
  (let* ((experiment (get-experiment))
	 (bibliography (gethash 'bibliography experiment))
	 (bibliography-keys (loop for k being hash-keys of bibliography collect k))
	 )
    (message "bibliography: %S\n" bibliography)
    (message "keys of experiment: %S" (loop for k being hash-keys of experiment collect k))
    (message "bibliography hash keys: %S" bibliography-keys)
    (should (member 'bibliography (loop for k being hash-keys of experiment collect k)))
    (should (hash-table-p bibliography))
    (should (member 'authors bibliography-keys))
    ))
