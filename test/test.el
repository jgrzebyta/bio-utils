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

;; test initialising taken from article http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html
;; not yet

(ert-deftest test-get-sequence ()
  "Take experiment data from the external server."
  (let ((out-text (call-array-express "E-MEXP-130" 'experiments)))
    (message "length: %s" (length out-text))
    (should (> (length out-text) 0))))


;; (ert-deftest test-get-nodes ()
;;   "Take nodes from experiment 'E-MEXP-130'"
;;   (message "^^^^^^^^^^^^^^^^^^^^^^")
;;   (let ((experiment (get-experiment-array-express "E-MEXP-130")))
;;     (message "experiment node: %s\n" experiment)
;;     (should (listp experiment))))


(ert-deftest test-sampleattributes ()
  "Test parsing of sampleattributes nodes"
  (message "&&&&&&&&&&&&&&&&")
  (let* ((experiment (get-experiment-array-express "E-MEXP-130"))
	(sattr (assoc "sample-attribute" experiment)))
    (message "sample-attribute node: %S\n" (assoc "sample-attribute" experiment))
    (should (listp experiment)))
  )

(ert-deftest test-bibliography ()
  "Test parsing bibliography"
  )
