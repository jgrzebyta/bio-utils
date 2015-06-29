;; ERT tests
(provide 'test)

(require 'f)

(defconst test-source-path
  (f-parent (f-this-file)))

(defconst source-path
  (f-parent test-source-path))

;; simplest sollution
(load-file "../download.el")

(prin1 *data-sources*)

;; take all dirs apart .git
(f-directories source-path (lambda (x) (null (string-match "\\.git" x))))

(> 5 0)

(directory-files source-path t ".el$")
(let (())
  )
(load (concat (pwd) "download.el"))



(ert-deftest get-sequence ()
  "Take experiment data from the external server."
(message "output: !!!%s" (buffer-name (call-array-express "E-MEXP-130" 'experiments)))
)

(test)

(message "dir:%s" (pwd))
