(defvar discuss-ls-filename nil)

(defun discuss-ls (arg)
  "Display txn headers of the meeting.  If no ARG, lists from current trn to
last trn.
If ARG is positive, list last ARG transactions.
If ARG is negative, prompt for range to display."
  (interactive "P")
  (let ((start discuss-current-transaction) ;lower limit
	(finish (nth 4 discuss-current-meeting-info))) ;upper limit
    (if (and discuss-ls-filename
	     (file-exists-p discuss-ls-filename))
	(delete-file discuss-ls-filename))
    ;;  (setq discuss-ls-filename (make-temp-name (format "/tmp/discuss-ls-%s" 
    ;;						     discuss-current-meeting)))
    (setq discuss-ls-filename (make-temp-name (format "/tmp/discuss-ls-%s" 
						      (user-login-name))))
    (if arg
	(progn
	  ;; this listp/setq is needed since negative args are not passed
	  ;; as the head of a list.
	  (if (listp arg)
	      (setq arg (car arg)))
	  (if (< arg 0)
	      (let ((range (discuss-ls-get-params)))
		(setq start (car range)
		      finish (car (cdr range))))
	    (setq start (1+ (- finish arg))))))
	  
    (discuss-send-cmd (format "(ls %s %d %d %d %s)\n"
			      discuss-ls-filename
			      start
			      finish
			      0		; filter flags
			      discuss-current-meeting)
		      'discuss-end-list-mtg 'discuss-read-form)))

(defun discuss-ls-get-params ()
  (list (string-to-int (read-string "Beginning of range: "))
	(string-to-int (read-string "End of range: "))))

(defun discuss-end-list-mtg ()
  (message "done list mtg")
  (let ((retwin (selected-window)))
    (if (get-buffer "*discuss-ls*") (kill-buffer "*discuss-ls*"))
    (setq discuss-ls-buf
	  (get-buffer-create "*discuss-ls*"))
    (let ((pop-up-windows t))
      (pop-to-buffer discuss-ls-buf))
    ;; (discuss-ls-mode)
    (let ((buffer-read-only nil))
      (insert-file-contents discuss-ls-filename))
    (select-window retwin))
  )
