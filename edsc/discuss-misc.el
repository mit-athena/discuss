;;;
;;;	Copyright (C) 1990 by the Massachusetts Institute of Technology
;;;    	Developed by the MIT Student Information Processing Board (SIPB).
;;;    	For copying information, see the file mit-copyright.h in this release.
;;;
;;;	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-misc.el,v $
;;;	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-misc.el,v 1.2 1990-09-19 16:34:39 bjaspan Exp $
;;;
;;;  Emacs lisp code with random parts of the emacs discuss user interface
;;;  We may want to split out the mail functions into a separate file if
;;;  if they grow much more....
;;;  Written by Theodore Ts'o, Barry Jaspan, and Mark Eichin
;;;
;;; $Log: not supported by cvs2svn $
; Revision 1.1  90/09/19  16:26:15  bjaspan
; Initial revision
; 

;;
;; Here is the add and delete meetings code....
;;
(defun discuss-parse-meeting-announcement ()
  (let (host pathname arg-start)
    (save-excursion
      (goto-char (point-min))
      (if (not (search-forward "  Meeting Name:  " nil t))
	  (error "Not a meeting announcement."))
      (forward-line 1)
      (if (not (search-forward "  Host:          " nil t))
	  (error "Not a meeting announcement."))
      (setq arg-start (point))
      (end-of-line)
      (setq host (buffer-substring arg-start (point)))
      (forward-line 1)
      (if (not (search-forward "  Pathname:      " nil t))
	  (error "Not a meeting announcement."))
      (setq arg-start (point))
      (end-of-line)
      (setq pathname (buffer-substring arg-start (point)))
      (list host pathname))))

(defun discuss-add-mtg (host pathname)
  "Add a discuss meeting."
  (interactive
   (if (or current-prefix-arg
	   (not (eq (current-buffer) discuss-cur-mtg-buf)))
       (list (read-input "Host Name:")
	     (read-input "Pathname:"))
     (discuss-parse-meeting-announcement)))
  (message "Trying to add meeting....")
  (discuss-send-cmd (format "(am %s %s)\n"
			    host pathname)
		    'discuss-end-add-mtg 'discuss-read-form))

(defun discuss-end-add-mtg ()
  (setq discuss-meeting-list (vconcat discuss-meeting-list
				     (list discuss-form)))
  (save-excursion
    (set-buffer discuss-main-buffer)
    (goto-char (point-max))
     (let ((buffer-read-only nil))
       (insert "\n")
       (discuss-lsm-1 discuss-form)
       (goto-char (point-max))
       (backward-delete-char 1)))
  (message "%s meeting added." (cadr discuss-form)))

(defun discuss-del-mtg (&optional meeting)
  "Delete a discuss meeting"
  (interactive (list (if (eq (current-buffer) discuss-cur-mtg-buf)
			 (progn
			   (discuss-leave-mtg)
			   discuss-current-meeting)
		       (if (or current-prefix-arg
			       (not (equal (buffer-name) discuss-main-buffer))
			       (= (point) 1))
			 (completing-read "Meeting name:  "
					  discuss-meeting-completion-list
					  nil t "")))))
  (if (not meeting)
      (let ((curline (- (count-lines 1 (min (1+ (point)) (point-max))) 3)))
	(if (< curline 0)
	    (error "Not looking at a meeting."))
	(setq meeting (cadr (aref discuss-meeting-list
				  curline)))
	(if (not (yes-or-no-p (format "Are you sure you want to delete %s? " 
				      meeting)))
	    (error "Delete cancelled."))
	))
  (message "Deleting meeting %s...." meeting)
  (discuss-send-cmd (format "(dm %s)\n" meeting)
		    'discuss-end-del-mtg 'discuss-read-form))

(defun discuss-end-del-mtg ()
  (save-excursion
    (set-buffer discuss-main-buffer)
    (goto-char (point-min))
    (if (not (re-search-forward (concat " " (regexp-quote (car discuss-form))
					"\\(,\\|$\\)")
			     nil t))
	(error "Can't find meeting %s." (car discuss-form)))
    (beginning-of-line)
    (forward-char 1)
    (let ((buffer-read-only nil))
      (insert-char 32 1)
      (delete-char 1)
      (forward-char 2)
      (insert-char 68 1)
      (delete-char 1)))
  (message "Meeting %s deleted." (car discuss-form)))


;;
;; Here follows the mail-within discuss code....
;;

(defun discuss-get-from-addr ()
 (save-excursion
   (save-restriction
     (widen)
     (goto-char (point-min))
     (skip-chars-forward "[0-9] ")
      (let* ((point-from-1 (point))
	     (point-from-2 (progn (re-search-forward " ") (point))))
	(buffer-substring point-from-1 point-from-2)))))

(defun discuss-forward ()
  (interactive)
      (let* ((forward-buffer (current-buffer))
	     (subject (concat "[" (discuss-get-from-addr)
			      ": " (or (mail-fetch-field "Subject") "") "]")))
	;; If only one window, use it for the mail buffer.
	;; Otherwise, use another window for the mail buffer
	;; so that the Rmail buffer remains visible
	;; and sending the mail will get back to it.
	(if (if (one-window-p t)
		(mail nil nil subject)
	      (mail-other-window nil nil subject))
	    (save-excursion
	      (goto-char (point-max))
	      (forward-line 1)
	      (insert 
	       (format "\n\n------- Forwarded transaction\n\n"))
	      (save-excursion
		(insert "\n------- End forwarded transaction\n"))
	      (insert-buffer forward-buffer)))))

(defun discuss-reply-by-mail ()
  "Reply to the current discuss transaction with Emacs sendmail"
  (interactive)
  (let ((to (mh-get-field "To:"))
	(from (mh-get-field "From:"))
	(cc (mh-get-field "Cc:"))
	(subject (nth 11 discuss-current-transaction-info)))
    
    (if (and (> (length subject) 3)
	     (not (string-match "[Rr]e: " (substring subject 0 4))))
	(setq subject (concat "Re: " subject)))

    (mail-other-window nil from subject 
		       (concat "[" (int-to-string 
				    (car discuss-current-transaction-info))
			       "]")
		       (concat to ", " cc) discuss-cur-mtg-buf)
    ))

; Stolen from mh-e.el
(defun mh-get-field (field)
  ;; Find and return the value of field FIELD in the current buffer.
  ;; Returns the empty string if the field is not in the message.
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (search-forward field nil t)) "")
	  ((looking-at "[\t ]*$") "")
	  (t
	   (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
	   (let ((field (buffer-substring (match-beginning 1)
					  (match-end 1)))
		 (end-of-match (point)))
	     (forward-line)
	     (while (looking-at "[ \t]") (forward-line 1))
	     (backward-char 1)
	     (if (<= (point) end-of-match)
		 field
		 (format "%s%s"
			 field
			 (buffer-substring end-of-match (point))))))))) 
