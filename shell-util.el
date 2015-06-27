;; It is a place for developing new functionalities for Emacs
(provide 'shell-util)


(defun shell-command-on-lines (command &optional output-buffer)
"Execute shell command on each line"
  (interactive
   (let (string)
     (setq string (read-shell-command "Shell command on region: "))
     (list string current-prefix-arg)))
  (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  (message " applied command: %s" command)
  (message " current buffer name: %s" (buffer-name (current-buffer)))
  (let ((out-buffer (get-buffer-create 
		     (or output-buffer "*Shell Command Output*")) ;; create new buffer or return one if exists.
		    )
	(in-buffer (current-buffer))
	(in-line-content nil)
	(results-content nil))
	
    ;; follow in-buffer
          (message "point %d-%d   restriction: %d-%d" (point-min) (point-max) (region-beginning) (region-end))
    (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (message "point %d-%d   restriction: %d-%d" (point-min) (point-max) (region-beginning) (region-end))    ;;; TEST
      (message "I'm here and current buffer is: %s" (current-buffer))                                         ;;; TEST 
      (loop named main for current-start = (point-min) then (+ 1 current-end)
	    and current-end = (1- (or (search-forward "\n" nil t) (+ 1 (point-max))))
	    while (and (<= current-end (point-max)) (< current-start (point-max)) (> current-end current-start))
	    do (progn
		 (setq in-line-content (buffer-substring-no-properties current-start current-end))
		 (message "start: %d   end: %d" current-start current-end)                                   ;;; TEST
		 (message "test: |%s|" in-line-content)                                                      ;;; TEST
		 (with-temp-buffer
		   (message "current buffer: %s" (current-buffer))
		   ;; inside temporary buffer. At the beginning
		   ;;(goto-char (point-min))
		   (princ in-line-content (current-buffer))
		   ;; run shell-command
		   (message "current buffer content: =|%s|=" (buffer-string))
		   (message "command %s" command)
		   (setq result-content (shell-command-to-string command)))
		 (message "output results length is: %d" (length result-content))
		 ;; print results into output-buffer
		 (with-current-buffer out-buffer
		   (princ result-content (current-buffer))
		   (princ "\n" (current-buffer))
		 )))
      )))
  )
