;;;
;;;	Copyright (C) 1990 by the Massachusetts Institute of Technology
;;;    	Developed by the MIT Student Information Processing Board (SIPB).
;;;    	For copying information, see the file mit-copyright.h in this release.
;;;
;;;	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-misc.el,v $
;;;	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-misc.el,v 1.7 1996-04-12 22:16:18 ghudson Exp $
;;;
;;;  Emacs lisp code with random parts of the emacs discuss user interface
;;;  We may want to split out the mail functions into a separate file if
;;;  if they grow much more....
;;;  Written by Theodore Ts'o, Barry Jaspan, and Mark Eichin
;;;

; We use mail-fetch-field
(require 'mail-utils)

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
       (list (read-input "Host Name: ")
	     (read-input "Pathname: " "/usr/spool/discuss/"))
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
  ;; A hack so added meetings show up on the completion list.
  (setq discuss-meeting-completion-list
	(append (list (cons (cadr discuss-form) 0)
		      (cons (caddr discuss-form) 0))
		discuss-meeting-completion-list))
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

(defun discuss-forward-to-meeting ()
  "Forward a transaction to another discuss meeting."
  (interactive)
  (if (not discuss-cur-mtg-buf)
      (error "Not looking at a meeting."))
  
  (let ((subject (concat "[" (discuss-get-from-addr)
			 ": " (or (mail-fetch-field "Subject") "") "]"))
	(meeting (completing-read "Forward to meeting: "
				  discuss-meeting-completion-list
				  nil t ""))
	(trn-txt (concat "\n\n------- Forwarded transaction\n\n"
			 (buffer-substring (point-min) (point-max))
			 "\n------- End forwarded transaction\n")))
    (discuss-enter meeting 0 subject nil trn-txt)
    ))

(defun discuss-reply-by-mail ()
  "Reply to the current discuss transaction with Emacs sendmail."
  (interactive)
  (let ((to (discuss-fetch-mail-field "To"))
	(from (discuss-fetch-mail-field "From"))
	(cc (discuss-fetch-mail-field "Cc"))
	(subject (nth 11 discuss-current-transaction-info))
	(author (nth 12 discuss-current-transaction-info))
	(in-reply (concat "\"[" (int-to-string
			       (car discuss-current-transaction-info))
			  "] in "
			  (nth 1 discuss-current-meeting-info)
			  "\"")))
    
    (if (equal from "")
	(setq from author))
    
    (if (and (> (length subject) 3)
	     (not (string-match "[Rr]e: " (substring subject 0 4))))
	(setq subject (concat "Re: " subject)))

    (mail-other-window nil from subject in-reply
		       (cond ((equal to "") nil)
			     ((equal cc "") to)
			     (t (concat to ", " cc)))
		       discuss-cur-mtg-buf)
    ))

(defun discuss-fetch-mail-field (field)
  (let (p)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(if (re-search-forward "^[a-z]+:" nil t) nil
	  (error "Not looking at a mail-fed transaction!"))
	(beginning-of-line)
	(setq p (point))
	(goto-char (point-max))
	(re-search-backward "^[a-z]+:")
	(re-search-forward "^$")
	(narrow-to-region p (point))
	(or (mail-fetch-field field nil t)
	    "")
	))))
