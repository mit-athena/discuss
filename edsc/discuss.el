;;;	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss.el,v $
;;;	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss.el,v 1.4 1988-10-26 23:25:47 srz Exp $
;;;
;;;  Emacs lisp code to remote control a "discuss" shell process to
;;;  provide an emacs-based interface to the discuss conferencing system.
;;;
;;;  Copyright (c) 1986 by the Massachusetts Institute of Technology.
;;;  Written by Stan Zanarotti and Bill Sommerfeld.
;;;
;;;  $Log: not supported by cvs2svn $
; Revision 1.3  88/10/26  15:23:15  eichin
; fix path for non-exl edsc client.
; 
; Revision 1.2  88/10/26  15:17:01  eichin
; Added discuss-trn-output
; 
; Revision 1.1  88/10/24  22:32:58  srz
; Initial revision
; 
;;;	Revision 1.3  86/12/10  16:12:30  srz
;;;	Some different keybindings
;;;
;;;	Revision 1.2  86/12/10  15:41:44  wesommer
;;;	Added autoload and keybinding for reply-by-mail.
;;;
;;;	Revision 1.1  86/12/09  16:09:20  wesommer
;;;	Initial revision
;;;
;;;

(provide 'discuss)

(require 'backquote)

(autoload 'make-shell "shell" "" t)

(autoload 'discuss-talk "discuss-enter"
	  "Enter a new discuss transaction." t)

(autoload 'discuss-reply "discuss-enter"
	  "Reply to an existing discuss transaction." t)

(defvar discuss-mtgs-mode-map nil
  "Keymap used by the meetings-list mode of the discuss subsystem")

(defvar discuss-list-mode-map nil
  "Keymap used by the transaction-list mode of the discuss subsystem")

(defvar discuss-trn-mode-map nil
  "Keymap used by the transaction mode of the discuss subsystem")

(defvar discuss-main-buffer "*meetings*"
  "Name of main buffer for discuss, which holds a list of the current
meetings")

(defvar discuss-cur-mtg-buf nil
  "Name of buffer for current Discuss meeting")

;;
;;  Determine pathname for subprocess.  Pretty gross, if you ask me.
;(defvar discuss-pathname (format "/mit/discuss/exl/edsc.%s"
;				 (if (equal emacs-build-system "paris")
;				     "vax"
;				   "rt"))
;  "*Name of program to run as slave process for discuss.")
(defvar discuss-pathname (format "/mit/discuss/%s/edsc/edsc"
				 (if (equal emacs-build-system "paris")
				     "vax"
				   "rt"))
  "*Name of program to run as slave process for discuss.")

;;; Random state variables:

;;; The buffer used to communicate with the slave process.
(setq discuss-shell-buffer nil)

;;; Hook to call when the current request to discuss has finished.
(setq discuss-cont nil)

;;; Is there currently a request to the slave outstanding?
(setq discuss-in-progress nil)

;;; Form returned from subprocess.
(setq discuss-form nil)

;;; Meeting list
(setq discuss-meeting-list nil)

;;; Global variable for show
(setq discuss-show-num 0)


;;; Major modes defined by this package.

;;; List of all meetings.

(defun discuss-mtgs-mode ()
  "Major mode for providing an emacs discuss subsystem.
This looks a lot like RMAIL.  This works by using edsc as a subjob.

The following commands are available:
n	go to next line.
p	go to previous line.
l	list meetings.
g	go to meeting listed on line.
q	Quit Discuss mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'discuss-mtgs-mode)
  (setq mode-name "Discuss (meetings)")
;  (setq mode-line-format
;	"--%1*%1*-Emacs: %17b   %M   %[(%m)%]----%3p--%-")
  (use-local-map discuss-mtgs-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'discuss-mode-hooks))

;;; Meeting list mode.

(defun discuss-list-mode ()
  "Major mode for looking at listings of a meeting under the
discuss subsystem."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'discuss-list-mode)
  (setq mode-name "Discuss (list)")
  (use-local-map discuss-list-mode-map)
  (setq buffer-read-only t)
  (make-local-variable 'trans-buffer)
  (setq trans-buffer nil)
  (make-local-variable 'meeting-name)
  (setq meeting-name nil)
  (run-hooks 'discuss-list-hooks))

;;; Transaction mode.

(defun discuss-trn-mode ()
"Major mode for looking at transactions of a meeting under the
discuss subsystem.
All normal editing commands are turned off.
Instead, these commands are available:

SPC	Scroll to next screen of this transaction.
DEL	Scroll to previous screen of this transaction.
n	Move to Next transaction.
p	Move to Previous transaction.
M-n	Move to Next transaction in chain.
M-p	Move to Previous transaction in chain.
l	Move to Last transaction in meeting.
f	Move to First transaction in meeting.
g	Goto transaction.
q       Quit meeting.
r	Reply to this transaction.
t	Talk.  Enter a new transaction.
a	Add meeting.  Not implemented yet."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'discuss-trn-mode)
  (setq mode-name "Discuss (transaction)")
  (use-local-map discuss-trn-mode-map)
  (setq buffer-read-only t)
  (make-local-variable 'discuss-current-transaction)
  (setq discuss-current-transaction 0)
  (make-local-variable 'discuss-highest-seen)
  (setq discuss-highest-seen 0)
  (run-hooks 'discuss-trn-hooks))


;;; Main entry point:  Start up a slave process and listen for requests.

(defun discuss ()
  "Enter discuss mode for emacs and list meetings."
  (interactive)

  (if discuss-shell-buffer
      nil
    (setq discuss-shell-buffer (make-shell "discuss-shell" discuss-pathname)))
    ;;; These values are innocuous for the sentinel and filter.
    ;    (discuss-send-cmd "" 'discuss-end-of-lsm 'into-discuss)

    (switch-to-buffer (get-buffer-create discuss-main-buffer))
    (discuss-mtgs-mode)
    (discuss-lsm))

;;; Entry points typically entered through key sequences.

(defun discuss-lsm ()
  "List discuss meetings"
  (interactive)
  (message "Listing meetings..."
  (switch-to-buffer (get-buffer discuss-main-buffer))
  (let ((buffer-read-only nil))
    (erase-buffer))
  (discuss-send-cmd "(gml)\n" 'discuss-end-of-lsm 'discuss-read-form)))

(defmacro cadr (x)
  (list 'car (list 'cdr x)))

(defmacro caddr (x)
  (` (car (cdr (cdr (, x))))))

(defmacro cddr (x)
  (` (cdr (cdr (, x)))))

(defun discuss-lsm-1 (entry)
  (insert (if (= (car entry) 1)
	      " c"
	    "  ")
	  "       "
	  (cadr entry))
  (if (cddr entry)
      (mapcar 'discuss-lsm-2 (cddr entry)))
  (insert "\n"))

(defun discuss-lsm-2 (name)
  (insert ", " name))

(defun discuss-end-of-lsm ()
  (message "done")
  (set-buffer discuss-main-buffer)
  (setq discuss-meeting-list (apply 'vector discuss-form))
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (insert " Flags  Meeting name\n"
	    " -----  ------------\n")
    (mapcar 'discuss-lsm-1 discuss-form)
    (goto-char (point-max))
    (backward-delete-char 1))
  (goto-char (point-min))
  (forward-line 2))

(defun discuss-quit ()
  "Exits Discuss mode."
  (interactive)
  (if discuss-cur-mtg-buf
      (discuss-leave-mtg))
  (kill-buffer discuss-main-buffer)
  (kill-buffer (buffer-name discuss-shell-buffer))
  (setq discuss-shell-buffer nil))

(defun goto-column (num)
  (insert-char 32 (max 0 (- num (current-column)))))

(defun discuss-goto (&optional meeting)
  "Go to a meeting"
  (interactive (list (if (or current-prefix-arg
			     (not (equal (buffer-name) discuss-main-buffer)))
			 (read-string "Meeting name:  "))))
  (if (not meeting)
      (let ((curline (- (count-lines 1 (1+ (point))) 3)))
	(if (< curline 0)
	    (error "Not looking at a meeting."))
	(setq meeting (cadr (aref discuss-meeting-list
				  curline)))))
  (setq discuss-cur-mtg-buf
	(get-buffer (concat "*" meeting " meeting*")))
  (if (not discuss-cur-mtg-buf)
      (progn
	(setq discuss-cur-mtg-buf
	      (get-buffer-create (concat "*" meeting " meeting*")))
	(switch-to-buffer discuss-cur-mtg-buf)
	(discuss-trn-mode)))
  (switch-to-buffer discuss-cur-mtg-buf)
  (setq discuss-current-meeting meeting)
  (discuss-send-cmd (format "(gmi %s)\n" meeting)
		    'discuss-end-of-goto 'discuss-read-form))

(defun discuss-end-of-goto ()
  (let ((last (nth 4 discuss-form))
	(highest-seen (nth 11 discuss-form)))
    (message "%s meeting: %d new, %d last."
	     (cadr discuss-form)
	     (- last highest-seen)
	     last)
    (set-buffer discuss-cur-mtg-buf)
    (setq discuss-current-meeting-info discuss-form)
    (setq discuss-highest-seen highest-seen)
    (cond ((not (zerop discuss-current-transaction)) nil)
	  ((zerop (nth 3 discuss-form)) (error "Empty meeting."))
	  ((or (zerop highest-seen) (>= highest-seen last))
	   (discuss-show-trn last))
	  (t (discuss-send-cmd (format "(nut %d %s)\n"
				       highest-seen
				       discuss-current-meeting)
			       'discuss-next-goto
			       'discuss-read-form)))))

(defun discuss-next-goto ()
  (discuss-show-trn (car discuss-form)))

(defun discuss-show-trn (trn-num)
  "Show transaction number N (prefix argument)."
  (interactive
   (list (if (not (numberp current-prefix-arg))
	     (string-to-int (read-string "Transaction number: "))
	   current-prefix-arg)))
  (if (and trn-num (numberp trn-num))
      (progn 
	(setq discuss-show-num trn-num)
	(discuss-send-cmd (format "(gti %d %s)\n"
				  discuss-show-num
				  discuss-current-meeting)
			  'discuss-show-current-1
			  'discuss-read-form))))
  
(defun discuss-show-current-1 nil
  (set-buffer discuss-cur-mtg-buf)
  (setq discuss-current-transaction-info discuss-form)
  (discuss-send-cmd (format "(gt %d %s)\n"
			    discuss-show-num
			    discuss-current-meeting)
		    'discuss-show-current-2
		    'discuss-read-trn))

(defun discuss-show-current-2 nil
  (set-buffer discuss-cur-mtg-buf)
  (setq discuss-current-transaction discuss-show-num)
  (setq discuss-highest-seen (max discuss-highest-seen discuss-current-transaction))
  (setq mode-line-process (format " %d/%d"
				  discuss-current-transaction
				  (nth 4 discuss-current-meeting-info)))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert discuss-text)
    (goto-char (point-min))))

(defun discuss-update ()
  "Update Discuss display to show new transactions"
  (interactive)
  (discuss-send-cmd (format "(gmi %s)\n" discuss-current-meeting)
		    'discuss-end-of-update 'discuss-read-form))

(defun discuss-end-of-update ()
  (setq discuss-current-meeting-info discuss-form)
  (set-buffer discuss-cur-mtg-buf)
  (discuss-show-trn discuss-current-transaction))

(defun discuss-next-trn ()
  "Show next transaction."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((next (caddr discuss-current-transaction-info)))
      (if (= next 0)
	  (error "No next transaction.")
	(discuss-show-trn next)))))

(defun discuss-prev-trn ()
  "Show previous transaction."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((prev (cadr discuss-current-transaction-info)))
      (if (= prev 0)
	  (error "No previous transaction.")
	(discuss-show-trn prev)))))

(defun discuss-nref ()
  "Show next transaction in chain."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((nref (nth 4 discuss-current-transaction-info)))
      (if (= prev 0)
	  (error "No next reference.")
	(discuss-show-trn nref)))))

(defun discuss-pref ()
  "Show previous transaction in chain."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((pref (nth 3 discuss-current-transaction-info)))
      (if (= pref 0)
	  (error "No previous reference.")
	(discuss-show-trn pref)))))

(defun discuss-first-trn ()
  "Show first transaction of meeting."
  (interactive)
  (let ((first (nth 3 discuss-current-meeting-info)))
	(discuss-show-trn first)))

(defun discuss-last-trn ()
  "Show last transaction of meeting."
  (interactive)
  (let ((last (nth 4 discuss-current-meeting-info)))
	(discuss-show-trn last)))

(defun discuss-leave-mtg ()
  "Leave the current discuss meeting"
  (interactive)
  (discuss-send-cmd (format "(ss %d %s)\n"
			    discuss-highest-seen
			    discuss-current-meeting))
  (kill-buffer (buffer-name discuss-cur-mtg-buf))
  (setq discuss-cur-mtg-buf nil)
  (switch-to-buffer discuss-main-buffer))

(defun discuss-format-trn-num (num)
  (format "[%s%d]"
	  (cond ((<= num 9) "000")
		((<= num 99) "00")
		((<= num 999) "0")
		(t ""))
	  num))

(defun region-to-string ()
  (buffer-substring (min (point) (mark)) (max (point) (mark))))


;;; Routines for communicating with slave process.  Since things are
;;; asynchronous when communicating with the process, we may have to
;;; spin on a flag if something else is in progress.
;;;
;;; The optional arguments to discuss-send-cmd are a function to be
;;; called by the filter-func when the end-of-operation is seen and a
;;; function to be used as a filter (it gets called with the process and
;;; a string when the process outputs something.
;;;
;;; It is possible that if discuss-in-progress gets set to true
;;; accidentally that things could get deadlocked; I think that can be
;;; avoided in the purely, but I'm not sure.. (knowing how easily elisp
;;; breaks unexpectedly, I should put in a lock timeout and a function to
;;; unlock things).

(defun discuss-send-cmd (cmd &optional end-func filter-func)
  ;; block until we have control over things..
  (discuss-block-til-ready)
  (if filter-func
      (setq discuss-in-progress t))
  (let ((proc (get-buffer-process discuss-shell-buffer)))
    (save-excursion 
      (set-buffer discuss-shell-buffer)
      (erase-buffer)
      (if end-func (setq discuss-cont end-func))
      (if filter-func (set-process-filter proc filter-func))
      (send-string proc cmd))))

(defun discuss-block-til-ready ()
  "Block, waiting until the previous operation for discuss finished"
  (if discuss-in-progress
      (progn 
	(while discuss-in-progress
	  (message "waiting for discuss...")
	  (sit-for 1)
	  (accept-process-output))
	(message ""))))

;;; Routines to filter the output from discuss.
;;; These are pretty simplistic

(defun discuss-read-form (process string)
  (let ((curbuf (current-buffer)))
    (set-buffer discuss-shell-buffer)
    (insert string)
    (if (equal (substring string -1 nil) "\n")
	(let ((end-of-line (string-match "\n" (buffer-string))))
	  (cond ((equal (buffer-substring 1 2) "-") ; warning
		 (message (buffer-substring 2 (1+ end-of-line)))
		 (delete-region 1 (+ 2 end-of-line))
		 (discuss-read-form process buffer-string))
		((equal (buffer-substring 1 2) ";") ; error
		 (message (buffer-substring 2 (1+ end-of-line)))
		 (ding)
		 (delete-region 1 (+ 2 end-of-line))
		 (setq discuss-in-progress nil))
		(t
		 (setq discuss-form (car (read-from-string (buffer-string))))
		 (setq discuss-in-progress nil)
		 (set-buffer curbuf)
		 (apply discuss-cont nil)))))))

(defun discuss-read-trn (process string)
  (let ((curbuf (current-buffer)))
    (set-buffer discuss-shell-buffer)
    (insert string)
    (if (equal (substring string -1 nil) "\n")
	(let ((end-of-line (string-match "\n" (buffer-string))))
	  (cond ((equal (buffer-substring 1 2) "-") ; warning
		 (message (buffer-substring 2 (1+ end-of-line)))
		 (delete-region 1 (+ 2 end-of-line))
		 (discuss-read-form process buffer-string))
		((equal (buffer-substring 1 2) ";") ; error
		 (message (buffer-substring 2 (1+ end-of-line)))
		 (ding)
		 (delete-region 1 (+ 2 end-of-line))
		 (setq discuss-in-progress nil))
		(t
		 (setq discuss-form (car (read-from-string (buffer-string))))
		 (if (> (count-lines (point-min) (point-max))
			(car discuss-form))
		     (progn
		       (setq discuss-text (buffer-substring (+ end-of-line 2) (point-max)))
		       (setq discuss-in-progress nil)
		       (set-buffer curbuf)
		       (apply discuss-cont nil)))))))))

(defun discuss-read-nil (process string)
  (setq discuss-in-progress nil))


;;; Keymaps, here at the end, where the trash belongs..

(if discuss-mtgs-mode-map
    nil
  (setq discuss-mtgs-mode-map (make-keymap))
  (suppress-keymap discuss-mtgs-mode-map)
  (define-key discuss-mtgs-mode-map "n" 'next-line)
  (define-key discuss-mtgs-mode-map "p" 'previous-line)
  (define-key discuss-mtgs-mode-map " " 'discuss-next-meeting)
  (define-key discuss-mtgs-mode-map "l" 'discuss-lsm)
  (define-key discuss-mtgs-mode-map "g" 'discuss-goto)
  (define-key discuss-mtgs-mode-map "q" 'discuss-quit)
  (define-key discuss-mtgs-mode-map "?" 'describe-mode))

(if discuss-trn-mode-map
    nil
  (setq discuss-trn-mode-map (make-keymap))
  (suppress-keymap discuss-trn-mode-map)
  (define-key discuss-trn-mode-map "." 'discuss-update)
  (define-key discuss-trn-mode-map " " 'scroll-up)
  (define-key discuss-trn-mode-map "\177" 'scroll-down)
  (define-key discuss-trn-mode-map "n" 'discuss-next-trn)
  (define-key discuss-trn-mode-map "p" 'discuss-prev-trn)
  (define-key discuss-trn-mode-map "\en" 'discuss-nref)
  (define-key discuss-trn-mode-map "\ep" 'discuss-pref)
  (define-key discuss-trn-mode-map "g" 'discuss-show-trn)
  (define-key discuss-trn-mode-map "f" 'discuss-first-trn)
  (define-key discuss-trn-mode-map "l" 'discuss-last-trn)
  (define-key discuss-trn-mode-map "h" 'discuss-trn-summary)
  (define-key discuss-trn-mode-map "\e\C-h" 'discuss-trn-summary)
  (define-key discuss-trn-mode-map "t" 'discuss-talk)
  (define-key discuss-trn-mode-map "r" 'discuss-reply)
  (define-key discuss-trn-mode-map "\er" 'discuss-reply-by-mail)
  (define-key discuss-trn-mode-map "\C-o" 'discuss-trn-output)
  (define-key discuss-trn-mode-map "i" 'discuss-trn-input)
  (define-key discuss-trn-mode-map "q" 'discuss-leave-mtg)
  (define-key discuss-trn-mode-map "?" 'describe-mode)
;  (define-key discuss-trn-mode-map "\C-d" 'discuss-trn-delete-backward)
)

;;; discuss-trn-output mostly stolen from rmail-output...
;;; converted by [eichin:19881026.1505EST]
(defvar discuss-output-last-file "discuss.out"
  "*Default file for discuss saves")

(defun discuss-trn-output (file-name)
  "Append this message to file named FILE-NAME."
  (interactive
   (list
    (read-file-name
     (concat "Output message to Unix mail file: (default "
	     (file-name-nondirectory discuss-output-last-file)
	     ") ")
     (file-name-directory discuss-output-last-file)
     discuss-output-last-file)))
  (setq file-name (expand-file-name file-name))
  (setq discuss-output-last-file file-name)
  (let ((discuss-trn-buf (current-buffer))
	(tembuf (get-buffer-create " discuss-trn-output"))
	(case-fold-search t))
    (save-excursion
      (set-buffer tembuf)
      (erase-buffer)
      (insert-buffer-substring discuss-trn-buf)
      (goto-char (point-max))
      (insert "\n")			;other modifying here as well
      (append-to-file (point-min) (point-max) file-name))
    (kill-buffer tembuf)))

