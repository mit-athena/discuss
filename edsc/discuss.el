;;
;; WARNING --- change discuss-source-dir when installing this file! 
;; 

(defvar discuss-source-dir "/usr/athena/lib/elisp"
  "Source directory which this version of discuss is loaded from.")

;;;
;;;	Copyright (C) 1989 by the Massachusetts Institute of Technology
;;;    	Developed by the MIT Student Information Processing Board (SIPB).
;;;    	For copying information, see the file mit-copyright.h in this release.
;;;
;;;	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss.el,v $
;;;	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss.el,v 1.31 1991-07-30 20:06:59 lwvanels Exp $
;;;
;;;  Emacs lisp code to remote control a "discuss" shell process to
;;;  provide an emacs-based interface to the discuss conferencing system.
;;;
;;;  Copyright (c) 1986 by the Massachusetts Institute of Technology.
;;;  Written by Stan Zanarotti, Bill Sommerfeld and Theodore Ts'o.
;;;
;;;  $Log: not supported by cvs2svn $
; Revision 1.30  91/07/22  11:19:52  lwvanels
; fixed location of edsc
; 
; Revision 1.29  91/04/26  17:24:17  raeburn
; Don't use ptys for subprocesses.
; 
; Revision 1.28  91/03/12  17:57:40  tytso
; Cleaned up error handling for discuss-catchup.  Calling discuss-goto-error
; was a bad move!
; 
; Have discuss-goto check to see whether or not it has read access to
; the meeting.
; 
; Revision 1.27  91/03/09  22:34:09  tytso
; Fixed stupid mistake in setting discuss-old-ss
; 
; Revision 1.26  91/03/09  16:33:49  tytso
; *** empty log message ***
; 
; Revision 1.25  91/03/08  21:05:33  tytso
; Fixed locking problem with the changed behavior of the "ss" command in
; edsc protocol version 2.4
; 
; Revision 1.24  91/02/24  14:46:58  bjaspan
; fixed my brain-dead discuss-catchup implementation
; 
; Revision 1.23  91/02/19  16:13:50  bjaspan
; removed useless check for machine-type in (edsc-machine-type).
; 
; Revision 1.22  91/02/01  17:45:59  tytso
; Removed useless let in mainline discuss entrance point.
; 
; Changed message and removed bell when emacs-discuss tries to update
; the changed flag for a meeting and fails (usually because the list of
; meetings isn't in the buffer at the moment).
; 
; Revision 1.21  91/01/04  19:30:46  bjaspan
; added doc for l and F to discuss-trn-mode.
; 
; Revision 1.20  91/01/03  22:23:31  bjaspan
; added discuss-set-seen-and-leave-mtg and discuss-catchup
; 
; Revision 1.19  90/12/05  16:23:50  raeburn
; Removed one-second delay at startup.  Argument to `discuss' is now
; optional, and no longer causes reversion to previous buffer.  Error
; messages now show up associated with meeting names, rather than
; showing up in the minibuffer and then disappearing.
; 
; Revision 1.18  90/11/05  14:46:02  eichin
; moved autoload path dependencies into discuss-source-dir variable.
; 
; Revision 1.17  90/11/01  21:32:56  eichin
; changed cadr, caddr, cddr to functions - the macros were breaking
; things in several places, particularly wrt byte-compiling.
; 
; Revision 1.16  90/10/29  22:55:04  bjaspan
; fixed a stupid bug in discuss-delete-trn
; 
; Revision 1.15  90/09/19  16:29:02  bjaspan
; merged my changes: meeting name completion, discuss-safe-delete
; 
; Revision 1.1  89/02/25  00:12:20  tytso
; Initial revision
;
; Revision 1.14  89/12/07  22:12:12  raeburn
; revamped mechanism for determining machine type
; 
; Revision 1.13  89/11/12  20:54:04  raeburn
; Pointed `edsc' to /mit/discuss/exl, where a pmax version exists also,
; and checked to see if we're on a pmax.
; 
; Revision 1.12  89/06/02  23:45:11  srz
; Added standard copyright notice.
; 
; Revision 1.11  88/12/15  13:50:06  srz
; Add patch so that broken meetings won't lose information.
; 
; Revision 1.10  88/12/05  08:18:12  eichin
; added discuss-next-meeting. [previous fix to discuss-goto was
; harmless, but not a bug. I had loaded a different version of the
; function, oops...]
; 
; Revision 1.9  88/12/05  08:11:22  eichin
; Fixed ^O [discuss-output-last-file wasn't getting set, probably
; because discuss-send-cmd doesn't really return...] and tested it.
; 
; Revision 1.8  88/11/08  07:28:26  raeburn
; typo fixes...
; 
; Revision 1.7  88/11/08  06:50:38  raeburn
; Fixed nref code not to reference variable "prev"; changed
; output-transaction routine to default to <mtg-name>.trans.
; 
; Revision 1.6  88/11/08  06:24:10  raeburn
; Removed some old comments; made "C-u M-x discuss" not list meetings,
; but accept meeting name; caused quit to send "(quit)" and disown
; process before killing buffer.  Also defined discuss-version function
; and variable with RCS id, misc other tweaks.
; 
; Revision 1.5  88/10/29  01:47:34  balamac
; Added randrp support.
; 
; Revision 1.4  88/10/26  23:25:47  srz
; Now goes to next transaction when going to a meeting.
; 
; Revision 1.3  88/10/26  15:23:15  eichin
; fix path for non-exl edsc client.
; 
; Revision 1.2  88/10/26  15:17:01  eichin
; Added discuss-trn-output
; 
; Revision 1.1  88/10/24  22:32:58  srz
; Initial revision
; 

(provide 'discuss)

(defvar discuss-DWIM nil
  "If true, enable Do_What_I_Mean mode.  Allows the user to read discuss by
repeatedly hitting the space bar.  For the truly discrimining and lazy 
user.")

(defvar discuss-safe-delete nil
  "If true, discuss asks for confirmation before deleting a transaction with
discuss-delete-trn.")

(defvar discuss-mtgs-mode-map nil
  "Keymap used by the meetings-list mode of the discuss subsystem")

(defvar discuss-list-mode-map nil
  "Keymap used by the transaction-list mode of the discuss subsystem")

(defvar discuss-trn-mode-map nil
  "Keymap used by the transaction mode of the discuss subsystem")

(defvar discuss-main-buffer "*meetings*"
  "Name of main buffer for discuss, which holds a list of the current
meetings")

(defvar discuss-version nil "Version of discuss code loaded.")
(defun discuss-version nil (interactive) (message discuss-version))

(defvar discuss-process nil
  "Structure discribing the slave discuss subprocess.")

(defvar discuss-cont nil
  "Internal hook to call when discuss subprocess is done.")

(defvar discuss-unwind nil
  "Internal hook to call when discuss subprocess returns an error.")

(defvar discuss-in-progress nil
  "t if a request to the slave subprocess is outstanding")

(defvar discuss-form nil
  "Lisp form returned by the subprocess.")

(defvar discuss-meeting-list nil
  "Meeting list.")

(defvar discuss-meeting-completion-list nil
  "Meeting list changed into the right format for completion-read")

(defvar discuss-show-num 0
  "Current discuss transaction number")

(defvar discuss-meeting nil
  "Buffer-local variable containing the name of the meeting of a discuss
transaction buffer.  Nil means this buffer is not a discuss-transaction 
buffer.")

(defvar discuss-meeting-info nil
  "Buffer-local variable containing the info struction for the discuss
transaction buffer.")

(defvar discuss-cur-mtg-buf nil
  "Name of buffer for current Discuss meeting")

(defvar discuss-async t
  "*Run discuss commands asynchronously.

Currently ignored (always async).")

(defvar discuss-in-show-trn nil)

(defvar discuss-error nil
  "Contains error message returned by edsc process.  If nil, means last 
request completed successfully.")

(defvar discuss-pathname "/usr/athena/etc/edsc"
  "*Name of program to run as slave process for discuss.")


;;; Major modes defined by this package.

;;; List of all meetings.

(defun discuss-mtgs-mode ()
  "Major mode for providing an emacs discuss subsystem.
This looks a lot like RMAIL.  This works by using ``edsc'' as a subjob.

The following commands are available:
n	go to next line.
p	go to previous line.
SPC     go to next meeting that has unread transactions
DEL     go to previous meeting has unread transactions
l	list meetings.
g	go to meeting listed on line.
a	add meeting
d	delete meeting
c	mark a meeting as read (catch up)
q	Quit Discuss mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'discuss-mtgs-mode)
  (setq mode-name "Discuss (meetings)")
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
>	Move to Last transaction in meeting.
<	Move to First transaction in meeting.
g	Goto transaction.
d	Delete transaction (and move forwards).
C-d	Delete transaction (and move backwards).
R	Retrieve transaction
q       Quit meeting.
r	Reply to this transaction.
f	Forward this transaction via mail.
t	Talk.  Enter a new transaction.
c	Catch up.  Mark the rest of the transactions in this meeting as read.
l	Mark transaction as highest-seen and leave meeting.
F	Toggle the flag on the current transaction.
a	Add meeting."
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
  (make-local-variable 'discuss-output-last-file)
  (make-local-variable 'discuss-meeting)
  (make-local-variable 'discuss-meeting-info)
  (setq discuss-output-last-file nil)
  (run-hooks 'discuss-trn-hooks))


;;; Main entry point:  Start up a slave process and listen for requests.

(defun discuss (&optional arg)
  "Enter discuss mode for emacs and list meetings."
  (interactive "P")
  (message "Starting discuss....")
  (if (not (and (get-buffer discuss-main-buffer)
		(buffer-name (get-buffer discuss-main-buffer))
		(> (length discuss-meeting-list) 0)))
      (progn
	(switch-to-buffer (get-buffer-create discuss-main-buffer))
	(discuss-mtgs-mode)
	(setq discuss-meeting-list nil)
	(if arg
	    (message "Hit `g' and enter a meeting name.")	      
	  (discuss-lsm)))
    (switch-to-buffer discuss-main-buffer)))

;;; Entry points typically entered through key sequences.

(defun discuss-list-meetings ()
  "List discuss meetings"
  (interactive)
  (message "Listing meetings..."
  (switch-to-buffer (get-buffer discuss-main-buffer))
  (let ((buffer-read-only nil))
    (erase-buffer))
  (discuss-send-cmd "(gml)\n" 'discuss-end-of-lsm 'discuss-read-form)))

(fset 'discuss-lsm (symbol-function 'discuss-list-meetings))

;;(defmacro cadr (x)
;;  (list 'car (list 'cdr x)))
(defun cadr (x) (car (cdr x)))

;;(defmacro caddr (x)
;;  (` (car (cdr (cdr (, x))))))
(defun caddr (x) (car (cdr (cdr x))))

;;(defmacro cddr (x)
;;  (` (cdr (cdr (, x)))))
(defun cddr (x) (cdr (cdr x)))

(defun discuss-lsm-1 (entry)
  (insert (cond ((eq (car entry) 1)
		 " c       ")
		((stringp (car entry))
		 "   X     ")
		(t
		 "         "))
	  (cadr entry))
  (cond ((stringp (car entry))
	 (insert " (" (car entry) ")"))
	((cddr entry)
	 (mapcar 'discuss-lsm-2 (cddr entry))))
  (insert "\n"))

(defun discuss-lsm-2 (name)
  (insert ", " name))

;; Compliments of jik
(defun flatten (list)
  (if (eq nil list) '())
  (let* ((newlist (copy-list (car list)))
        (restoflist (cdr list))
        (pointer newlist))
    (while (not (eq nil restoflist))
      (while (not (eq nil (cdr pointer)))
        (setq pointer (cdr pointer)))
      (setcdr pointer (copy-list (car restoflist)))
      (setq restoflist (cdr restoflist)))
    newlist))

(defun copy-list (list)
  (if (eq nil list) 
      '()
    (cons (car list) (copy-list (cdr list)))))

(defun discuss-end-of-lsm ()
  (message "Listing meetings...done.")
  (let ((orig-buffer (current-buffer)))
    (set-buffer discuss-main-buffer)
    (setq discuss-meeting-list (apply 'vector discuss-form))
    (setq discuss-meeting-completion-list
	  (mapcar (function (lambda (x) (cons x 0)))
		  (flatten (mapcar 'cdr discuss-form))))
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (insert " Flags   Meeting name\n"
	      " -----   ------------\n")
      (mapcar 'discuss-lsm-1 discuss-form)
      (goto-char (point-max))
      (backward-delete-char 1))
    (goto-char (point-min))
    (forward-line 2)
    (set-buffer orig-buffer)))

; Not working yet...
(defun discuss-find-meeting (meeting)
  (let ((i 0)
	(eol (length discuss-meeting-list)))
    (while (and (< i eol)
		(not (memq meeting (aref discuss-meeting-list i))))
	(setq i (1+ i)))
    (if (< i eol)
	i
      nil)))
	

(defun discuss-quit ()
  "Exits Discuss mode."
  (interactive)
  (if discuss-cur-mtg-buf
      (discuss-leave-mtg))
  (discuss-restart)
  (switch-to-buffer (other-buffer))
  (bury-buffer discuss-main-buffer))

(defun discuss-goto (&optional meeting)
  "Go to a meeting"
  (interactive (list (if (or current-prefix-arg
			     (not (equal (buffer-name) discuss-main-buffer))
			     (= (point) 1))
			 (completing-read "Meeting name:  "
					  discuss-meeting-completion-list
					  nil t ""))))
  (if (not meeting)
      (let ((curline (- (count-lines 1 (min (1+ (point)) (point-max))) 3)))
	(if (< curline 0)
	    (error "Not looking at a meeting."))
	(setq meeting (cadr (aref discuss-meeting-list
				  curline)))))
  (if (not (and discuss-cur-mtg-buf
	       (buffer-name discuss-cur-mtg-buf)
	       (equal discuss-current-meeting meeting)))
      (progn
	(if discuss-cur-mtg-buf
	    (discuss-leave-mtg))
	(setq discuss-cur-mtg-buf
	      (get-buffer-create (concat "*" meeting " meeting*")))
	(switch-to-buffer discuss-cur-mtg-buf)
	(discuss-trn-mode))
    (progn
      (set-buffer discuss-cur-mtg-buf)
      (discuss-send-cmd (format "(ss %d %s)\n"
				discuss-highest-seen
				discuss-meeting)
			nil
			(if discuss-old-ss nil 'discuss-read-form))))
  (switch-to-buffer discuss-cur-mtg-buf)
  (setq discuss-meeting meeting)
  (setq discuss-current-meeting meeting)   ;;; denigrated
  (setq discuss-output-last-file (concat discuss-meeting ".trans"))
  (discuss-send-cmd (format "(gmi %s)\n" meeting)
		    'discuss-end-of-goto 'discuss-read-form 
		    'discuss-goto-error))

(defun discuss-goto-error ()
  "Called to back out when there's an error in going to a meeting."
  (kill-buffer (buffer-name discuss-cur-mtg-buf))
  (setq discuss-cur-mtg-buf nil)
  (setq discuss-current-meeting nil)
  (switch-to-buffer discuss-main-buffer))

(defun discuss-end-of-goto ()
  (let ((last (nth 4 discuss-form)))
    ;
    ; To have gotten this far, we must have had status access....
    ;
    (if (not (string-match "r" (nth 10 discuss-form)))
	(progn
	  (discuss-goto-error)
	  (error "Insufficient access to read transactions in %s"
		 (nth 1 discuss-current-meeting-info))))
    (setq discuss-highest-seen (nth 11 discuss-form))
    (if (> discuss-highest-seen last)
	(progn
	  (beep)
	  (message "Warning!  Last seen transaction higher than last transaction")
	  (sit-for 1)
	  (setq discuss-highest-seen last)))
    (message "%s meeting: %d new, %d last."
	     (cadr discuss-form)
	     (- last discuss-highest-seen)
	     last)
    (set-buffer discuss-cur-mtg-buf)
    (setq discuss-current-meeting-info discuss-form)
    (cond ((not (zerop discuss-current-transaction)) nil)
	  ((zerop (nth 3 discuss-form)) (error "Empty meeting."))
	  ((or (zerop discuss-highest-seen) (>= discuss-highest-seen last))
	   (discuss-show-trn last))
	  (t (discuss-send-cmd (format "(nut %d %s)\n"
				       discuss-highest-seen
				       discuss-meeting)
;			       'discuss-next-goto
			       '(lambda () (progn (discuss-la-cache-transaction
						   (car discuss-form))
						  (discuss-block-til-ready nil)
						  (discuss-show-trn 
						   (car discuss-form))))
			       'discuss-read-form)))))

;(defun discuss-next-goto ()
;  (discuss-show-trn (car discuss-form)))

;;;
;;; This is broken --- only works in transaction buffer.  Of course, this is
;;; is the only place that you'd really want to use it.  Actually, it
;;; sort of works in meetings-mode too, but only barely so.
;;; This should be cleaned up.  -- TYT
;;;
(defun discuss-stat (&optional meeting)
  "Go to a meeting"
  (interactive (list (if (eq (current-buffer) discuss-cur-mtg-buf)
			 discuss-meeting
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
				  curline)))))
  (discuss-send-cmd (format "(gmi %s)\n" meeting)
		    'discuss-end-of-stat 'discuss-read-form))

(defun discuss-end-of-stat ()
  (setq discuss-current-meeting-info discuss-form)
  (setq discuss-meeting-info discuss-form)
  (let ((last (nth 4 discuss-form))
	(highest-seen (nth 11 discuss-form)))
    (if (and (equal (cadr discuss-form) discuss-current-meeting)
	     (not (= discuss-highest-seen 0)))
	(setq highest-seen discuss-highest-seen))    ;; This is wrong...
    (message "%s meeting: %d new, %d last."
	     (cadr discuss-form)
	     (- last highest-seen)
	     last)))

(defun discuss-show-trn (trn-num)
  "Show transaction number N (prefix argument)."
  (interactive
   (list (if (not (numberp current-prefix-arg))
	     (string-to-int (read-string "Transaction number: "))
	   current-prefix-arg)))
  (if (and trn-num (numberp trn-num))
      (progn 
	(setq discuss-show-num trn-num)
	(setq discuss-la-to-cache nil)	; invalidate to do list
	(setq discuss-la-next-chaining 0)
	(let ((discuss-cache-file (discuss-la-get-art trn-num)))
	  (if (not discuss-cache-file)
	      (progn
		(discuss-la-cache-transaction trn-num)
		(setq discuss-cache-file (discuss-la-get-art trn-num))
		(if (not discuss-cache-file)
		    (error "Can't find transaction %d in cache!!!" trn-num))))
	  ;
	  ; OK, start queueing next transaction to be cached.
	  ;
	  (setq discuss-la-next-trn trn-num)
	  (setq discuss-la-next-chaining discuss-max-next-chaining)

	  (set-buffer discuss-cur-mtg-buf)
	  (setq discuss-current-transaction-info discuss-form)
	  (setq discuss-current-transaction discuss-show-num)
	  (setq discuss-highest-seen (max discuss-highest-seen
					  discuss-current-transaction))
	  (if (>= discuss-current-transaction
		     (nth 4 discuss-current-meeting-info))
	      (let ((discuss-in-show-trn t))
		(discuss-update)
		(discuss-block-til-ready nil)))

	  (setq mode-line-process (format " %d/%d"
					  discuss-current-transaction
					  (nth 4 discuss-current-meeting-info)))
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (insert-file-contents discuss-cache-file)
	    (goto-char (point-min)))
	  (if (= (caddr discuss-current-transaction-info) 0)
	      (progn
		(message "Last transaction in %s" discuss-meeting)
		(discuss-mark-read-meeting discuss-meeting)
		(discuss-next-meeting t)
		))
	  (discuss-la-do-cache)
	  ))))

(defun discuss-update ()
  "Update Discuss display to show new transactions"
  (interactive)
  (discuss-send-cmd (format "(gmi %s)\n" discuss-meeting)
		    'discuss-end-of-update 'discuss-read-form))

(defun discuss-end-of-update ()
  (setq discuss-current-meeting-info discuss-form)
  (save-excursion
    (set-buffer discuss-cur-mtg-buf)
    (if (not discuss-in-show-trn)
	(discuss-show-trn discuss-current-transaction))))

(defun discuss-next-trn ()
  "Show next transaction."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((next (caddr discuss-current-transaction-info)))
      (if (= next 0)
	  (progn
	    (discuss-la-invalidate discuss-current-transaction)
	    (discuss-la-cache-transaction discuss-current-transaction)
	    (discuss-la-block-til-ready)
	    (let ((cache-entry (discuss-la-search discuss-current-transaction)))
	      (if (and cache-entry
		       (caddr cache-entry))
		    (setq discuss-current-transaction-info (caddr cache-entry))))
	    (if (= 0 (caddr discuss-current-transaction-info))
		(error "No next transaction.")
	      (discuss-next-trn)))
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
      (if (= nref 0)
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

(defun discuss-lref ()
  "Show last transaction in chain."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((lref (nth 6 discuss-current-transaction-info)))
      (if (= lref 0)
	  (error "No last reference.")
	(discuss-show-trn lref)))))

(defun discuss-fref ()
  "Show first transaction in chain."
  (interactive)
  (if (or (not discuss-current-transaction)
	  (= discuss-current-transaction 0))
      (error "Not looking at transactions")
    (let ((fref (nth 5 discuss-current-transaction-info)))
      (if (= fref 0)
	  (error "No first reference.")
	(discuss-show-trn fref)))))

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

(defun discuss-toggle-trn-flag ()
  "Toggle the per-transaction flag"
  (interactive)
  (let ((old-flag (nth 13 discuss-current-transaction-info)))
    (if old-flag
	(progn
	  (discuss-la-invalidate discuss-current-transaction)
	  (message "Toggling the transaction flag....")
	  (discuss-send-cmd (format "(sfl %d %d %s)\n" 
				    (logxor old-flag 2)
				    discuss-current-transaction
				    discuss-meeting)
			    'discuss-end-of-toggle 'discuss-read-form)))))

(defun discuss-end-of-toggle ()
  (discuss-show-trn discuss-current-transaction)
  (message ""))

(defun discuss-set-seen-and-leave-mtg (arg)
  "Sets the highest transaction number seen in the current meeting to
the argument or the current transaction and leaves the meeting."
  (interactive "p")
  (if (not discuss-cur-mtg-buf)
      (error "Not looking at a meeting.")
    (if current-prefix-arg
	(setq discuss-highest-seen arg)
      (setq discuss-highest-seen discuss-current-transaction))
    (discuss-leave-mtg)))

(defun discuss-leave-mtg ()
  "Leave the current discuss meeting"
  (interactive)
  (discuss-la-flush)
  (if (buffer-name discuss-cur-mtg-buf)
      (progn
	(set-buffer discuss-cur-mtg-buf)
	(if (not (= discuss-highest-seen 0))
	    (discuss-send-cmd (format "(ss %d %s)\n"
				      discuss-highest-seen
				      discuss-meeting)
			      nil
			      (if discuss-old-ss nil 'discuss-read-form)))
	(kill-buffer (buffer-name discuss-cur-mtg-buf))
	(setq discuss-cur-mtg-buf nil)
	(setq discuss-current-meeting nil)
	(switch-to-buffer discuss-main-buffer))))

(defun discuss-catchup (&optional meeting)
  "Mark all messages in the current meeting as read."
  (interactive
   (list (or discuss-cur-mtg-buf
	     (if (or current-prefix-arg (= (point) 1))
		 (completing-read "Meeting name:  "
				  discuss-meeting-completion-list
				  nil t "")))))

  ;; If meeting is nil or a string, we are in the *meetings* buffer.  Use the
  ;; meeting on the current line.
  (if (or (not meeting) (stringp meeting))
      (let ((curline (- (count-lines 1 (min (1+ (point)) (point-max))) 3)))
	(if meeting
	    nil
	  (if (< curline 0)
	      (error "Not looking at a meeting."))
	  (setq meeting (cadr (aref discuss-meeting-list curline))))
	(message "Catching up in %s" meeting)
	(discuss-send-cmd (format "(gmi %s)\n" meeting)
			  'discuss-end-of-catchup 'discuss-read-form))
    ;; Otherwise just set discuss-highest-seen.
    (setq discuss-highest-seen (nth 6 discuss-current-meeting-info))
    (discuss-mark-read-meeting (nth 1 discuss-current-meeting-info))
    (discuss-next-meeting t)
    (discuss-leave-mtg)
    ))

(defun discuss-end-of-catchup ()
  (let ((meeting (nth 1 discuss-form))
	(highest (nth 6 discuss-form)))
    (discuss-send-cmd (format "(ss %d %s)\n" highest meeting)
		      nil 
		      (if discuss-old-ss nil 'discuss-read-form))
    (discuss-mark-read-meeting meeting)
    (discuss-next-meeting t)
    (message "Done.")
    ))

(defun discuss-delete-trn-backwards (trn-num)
  (interactive
   (cond (current-prefix-arg
	  (if discuss-cur-mtg-buf
	      (list (string-to-int (read-input "Transaction to delete: ")))
	    (error "Not currently visiting a meeting.")))
	 ((eq (current-buffer) discuss-cur-mtg-buf)
	  (list discuss-current-transaction))
	 (t
	  (if discuss-cur-mtg-buf
	      (list (string-to-int (read-input "Transaction to delete: ")))
	    (error "Not currently visiting a meeting.")))
	 ))
  (discuss-delete-trn trn-num t))

(defun discuss-delete-trn (trn-num &optional backwards)
  (interactive
   (cond (current-prefix-arg
	  (if discuss-cur-mtg-buf
	      (list (string-to-int (read-input "Transaction to delete: ")))
	    (error "Not currently visiting a meeting.")))
	 ((eq (current-buffer) discuss-cur-mtg-buf)
	  (list discuss-current-transaction))
	 (t
	  (if discuss-cur-mtg-buf
	      (list (string-to-int (read-input "Transaction to delete: ")))
	    (error "Not currently visiting a meeting.")))
	 ))
  ;; Probably we should make sure the transaction can be deleted before
  ;; asking this question, but the cache code is too confusing...
  (if (and discuss-safe-delete
	   (not (yes-or-no-p (format "Delete transaction %d? " trn-num))))
      nil
    (let (info
	  other
	  (cache-entry (discuss-la-search trn-num)))
      (if (and cache-entry
	       (= discuss-current-transaction trn-num))
	  (progn
	    (setq info (caddr cache-entry))
	    (if backwards 
		(setq discuss-current-transaction (cadr info)
		      other (caddr info))
	      (setq discuss-current-transaction (caddr info)
		    other (cadr info)))
	    (if (= discuss-current-transaction 0)
		(setq discuss-current-transaction other))
	    (if (= discuss-current-transaction 0)
		(progn
		  (message "No more transactions in meeting!")
		  (beep)))
	    )))
    (discuss-la-invalidate trn-num)
    (discuss-la-invalidate-relatives trn-num)
    (message "Deleting %d...." trn-num)
    (discuss-send-cmd (format "(dt %d %s)\n" trn-num discuss-meeting)
		      'discuss-end-del-trn 'discuss-read-form)))

(defun discuss-end-del-trn ()
  (message "Done.")
  (discuss-show-trn discuss-current-transaction))

(defun discuss-retrieve-trn (trn-num)
  (interactive "nTransaction to retrieve: ")
  (setq discuss-current-transaction trn-num)
  (message "Retrieving %d...." trn-num)
  (discuss-send-cmd (format "(rt %d %s)\n" trn-num discuss-meeting)
		    'discuss-end-rt-trn 'discuss-read-form))

(defun discuss-end-rt-trn ()
  (message "Done.")
  (discuss-la-invalidate-relatives discuss-current-transaction)
  (discuss-show-trn discuss-current-transaction))

(defun discuss-la-invalidate-relatives (trn-num)
  (discuss-la-cache-transaction trn-num)
  (discuss-la-block-til-ready)
  (let (info
	(cache-entry (discuss-la-search trn-num)))
    (if cache-entry
	(progn
	  (setq info (caddr cache-entry))
	  (discuss-la-invalidate (caddr info))	; next
	  (discuss-la-invalidate (cadr info))	; prev
	  (discuss-la-invalidate (nth 3 info))	; pref
	  (discuss-la-invalidate (nth 4 info))  ; nref
	  (discuss-la-invalidate (nth 6 info))))) ; lref
  )

(defun discuss-format-trn-num (num)
  (format "[%s%d]"
	  (cond ((<= num 9) "000")
		((<= num 99) "00")
		((<= num 999) "0")
		(t ""))
	  num))

;;; Discuss DWIM
(defun discuss-scroll-up ()
  (interactive)
  (condition-case err
      (scroll-up nil)
    (error 
     (if (and discuss-DWIM
	      (equal err '(end-of-buffer)))
	 (if (equal discuss-current-transaction
		    (nth 4 discuss-current-meeting-info))
	     (discuss-leave-mtg)
	   (discuss-next-trn))
       (signal (car err) (cdr err))))))


;;; These routines attempt to do transaction lookahead, to speed the discuss
;;; reading process.
;;;

(defvar discuss-la-cache-dir "/tmp"
  "The directory of where to store the cached transaction.")

(defvar discuss-la-cache nil
  "List of cached discuss transactions.")

(defvar discuss-la-to-cache nil
  "List of transaction that should be cached.")

(defvar discuss-max-cache-length 32
  "Maximum number of cached transactions.")

(defvar discuss-max-next-chaining 5
  "Maximum number of look-ahead in the forward direction.")

(defvar discuss-la-next-chaining 0
  "Number of look ahead's we have left to do.")

(defvar discuss-la-next-trn 0
  "This points to the pref of the transaction that should be cached next.")

(defvar discuss-reading-form ""
  "This is a scratch variable where output from the discuss backend is 
accumulated.")

(defvar discuss-la-in-progress nil
  "Contains the cache entry that we're currently working on in lookahead-
article.  If it is set to nil, we will abort processing the 
lookahead-article.")

(defun discuss-la-flush nil
  "Flushes the look-ahead cache.  Required when you exit a meeting."
  (setq discuss-la-in-progress nil)
  (while discuss-la-cache
    (if (file-exists-p (cadr (car discuss-la-cache)))
	(delete-file (cadr (car discuss-la-cache))))
    (setq discuss-la-cache (cdr discuss-la-cache)))
  )

(defun discuss-la-block-til-ready ()
  "Block, waiting until the previous operation look-ahead operation completes"
  (if discuss-la-in-progress
      (progn 
	(while discuss-la-in-progress
	  (message "waiting for (la) discuss...")
	  (sit-for 1)
	  (accept-process-output))
	(message ""))))

(defun discuss-la-search (key)
  (let ((prev nil)
	(this discuss-la-cache))
    (while (and this
		(not (equal key (car (car this)))))
      (setq prev this)
      (setq this (cdr this)))
    ;;
    ;; This is horrible mutator hacking is done to keep
    ;; discuss-la-cache in LRU order
    ;;
    (if (and this prev)
	(progn
	  (setcdr prev (cdr this))
	  (setcdr this discuss-la-cache)
	  (setq discuss-la-cache this)))
    (car this)))
	
(defun discuss-la-invalidate (num)
  (let ((cache-entry (discuss-la-search num)))
    (if cache-entry
	(setcar (cdr (cdr cache-entry)) nil))))

(defun discuss-la-get-art (num)
  (discuss-la-block-til-ready)
  (let ((cache-entry (discuss-la-search num)))
    (setq discuss-form (nth 2 cache-entry))
    (if (and cache-entry
	     discuss-form)
	(progn
;	  (message "Cache hit!")
	  (setq discuss-show-num num)
	  (discuss-la-cache-transaction)
	  (cadr cache-entry))
      nil)))

(defun discuss-la-cache-transaction (&optional num)
  (if (and (not discuss-la-in-progress)
	   (not num)
	   discuss-la-to-cache)
      (setq num (car discuss-la-to-cache)
	    discuss-la-to-cache (cdr discuss-la-to-cache)))
  (if (and (not discuss-la-in-progress)
	   (not num)
	   (> discuss-la-next-chaining 0))
      (let ((cache-entry (assoc discuss-la-next-trn discuss-la-cache)))
	(if cache-entry
	    (progn
	      (setq discuss-la-next-chaining (1- discuss-la-next-chaining))
	      (setq num (caddr (caddr cache-entry)))	; Next trn to cache
	      (if (= num 0)
		  (progn
		    (setq num nil)
		    (setq discuss-la-next-chaining 0)))
	      (setq discuss-la-next-trn num))
	  (setq discuss-la-next-chaining 0
		discuss-la-next-trn 0))))
  (if num
      (let ((cache-entry (discuss-la-search num)))
	(if (not (and cache-entry
		      (nth 2 cache-entry)))
	    (progn
	      (discuss-la-block-til-ready)
;	      (message "Starting to cache %d" num)
	      (if cache-entry
		  (setq discuss-la-in-progress cache-entry)
		(progn
		  (setq discuss-la-in-progress 
			(list num 
			      (make-temp-name (format "%s/edsc%d"
						      discuss-la-cache-dir 
						      num
						      ))
			      nil))
		  (setq discuss-la-cache (cons discuss-la-in-progress
					       discuss-la-cache))))
	      (discuss-send-cmd (format "(gtf %s %d %s)\n"
					(cadr discuss-la-in-progress)
					num
					discuss-current-meeting)
				'discuss-la-cache-transaction-1
				'discuss-read-form
				'discuss-la-flush))
	  (progn
;	    (message "%s already cached." num)
	    (discuss-la-cache-transaction))))))

(defun discuss-la-cache-transaction-1 nil
  (if discuss-la-in-progress
      (progn
	(setcar (cdr (cdr discuss-la-in-progress)) discuss-form)
;	(message "Cache finished")
	;;
	;; If we have too many transactions in the cache, then
	;; we need to purge the least recently used transaction.
	;;
	(if (> (length discuss-la-cache) discuss-max-cache-length)
	    (let ((file (cadr (nth (1- (length discuss-la-cache)) 
				   discuss-la-cache))))
	      (if (file-exists-p file)
		  (delete-file file))
	      (setcdr (nthcdr (- (length discuss-la-cache) 2) 
			      discuss-la-cache) nil)))
	(setq discuss-la-in-progress nil)
	(discuss-la-cache-transaction)
	)))

(defun discuss-la-do-cache (&optional no-retry)
  (let ((next (caddr discuss-current-transaction-info))
	(prev (cadr discuss-current-transaction-info))
	(nref (nth 4 discuss-current-transaction-info))
	(pref (nth 3 discuss-current-transaction-info)))
    (if (> next 0)
	(setq discuss-la-to-cache (cons next discuss-la-to-cache)))
    (if (> prev 0)
	(setq discuss-la-to-cache (cons prev discuss-la-to-cache)))
    (if (> nref 0)
	(setq discuss-la-to-cache (cons nref discuss-la-to-cache)))
    (if (> pref 0)
	(setq discuss-la-to-cache (cons pref discuss-la-to-cache)))
    (discuss-la-cache-transaction)))

    


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

(defun discuss-restart ()
  "Used to save the world when edsc gets hung or dies...

Flushes the discuss cache and destroys the edsc process."
  (interactive)
  (discuss-la-flush)
  (if (and discuss-process
	   (equal (process-status discuss-process) 'run))
      (send-string discuss-process "(quit)\n"))
  (setq discuss-process nil
	discuss-in-progress nil))

(defun discuss-send-cmd (cmd &optional end-func filter-func unwind-func)
  "Send an command to the edsc process"
  (if (not discuss-process)
      (let ((process-conection-type nil))
	(if (not (file-exists-p discuss-pathname))
	    (error "%s does not exist!" discuss-pathname))
	(setq discuss-process (start-process "discuss-shell" 
					     nil
					     discuss-pathname))
	(set-process-sentinel discuss-process 'discuss-edsc-sentinel)
	(discuss-send-cmd "(gpv)\n" nil 'discuss-read-form)
	(discuss-block-til-ready t)
	(let* ((discuss-vers (cond (discuss-form
				    (car discuss-form))
				   ((equal discuss-error 
					  "Unimplemented operation")
				    10)
				   (t
				    (error "Edsc returned error: %s" 
					   discuss-error))))
	       (ver-string 
		(format "%d.%d" 
			(/ discuss-vers 10)
			(- discuss-vers (* (/ discuss-vers 10) 10)))))
	  (if (> 23 discuss-vers)
	      (setq discuss-version-string "")
	    (setq discuss-version-string (cadr discuss-form)))
	  (setq discuss-old-ss (= 23 discuss-vers))
	  (if (> 20 discuss-vers)
	      (progn
		(discuss-restart)
		(error "Bad version of edsc (%s) --- need version 2.0."
		       ver-string))
	    (progn
	      (message "Started edsc process.... version %s %s)" 
		       ver-string discuss-version-string)
	      (sit-for 1))))))
    
  ;; block until we have control over things..
  (discuss-block-til-ready t)
  (if filter-func
      (setq discuss-in-progress t))
  (save-excursion 
    (setq discuss-reading-string "")
    (setq discuss-cont end-func)
    (setq discuss-unwind unwind-func)
    (if filter-func (set-process-filter discuss-process filter-func))
    (send-string discuss-process cmd)))

(defun discuss-block-til-ready (verbose)
  "Block, waiting until the previous operation for discuss finished.
If VERBOSE is non-nil, then print a message that we're waiting for the 
discuss server while we spin-block."
  (if discuss-in-progress
      (progn 
	(while discuss-in-progress
	  (if verbose
	      (message "waiting for discuss..."))
	  (sit-for 1)
	  (accept-process-output))
	(message ""))))

;;;
;;; This gets called when something nasty has happened to our edsc.
;;;
(defun discuss-edsc-sentinel (process signal)
  (let ((buffer (process-buffer process))
	(status (process-status process)))
    (cond
     ((eq status 'exit)
      (discuss-restart))
     ((eq status 'signal)
      (ding)
      (message "discuss-shell: %s." 
	       (substring signal 0 -1))
      (discuss-restart))
     )))

;;; Routines to filter the output from discuss.
;;; These are pretty simplistic

(defun discuss-read-form (process string)
  (setq discuss-reading-string (concat discuss-reading-string string))
  (let* ((end-of-line (string-match "\n" discuss-reading-string)))
    (if end-of-line
	(let ((flag-char (substring discuss-reading-string 0 1))
	      (first-line (substring discuss-reading-string 1 
				     end-of-line)))
	  (setq discuss-error nil)
	  (cond ((equal flag-char "-") ; warning
		 (message first-line)
		 (setq discuss-reading-string
		       (substring discuss-reading-string (1+ end-of-line)))
		 (discuss-read-form process ""))
		((equal flag-char ";") ; error
		 (setq discuss-error first-line)
		 (message discuss-error)
		 (ding)
		 (setq discuss-reading-string
		       (substring discuss-reading-string (1+ end-of-line)))
		 (setq discuss-in-progress nil)
		 (setq discuss-form nil)
		 (if discuss-unwind
		     (apply discuss-unwind nil)))
		(t
		 (setq discuss-form 
		       (car (read-from-string (concat "(" first-line))))
		 (setq discuss-in-progress nil)
		 (if discuss-cont
		     (apply discuss-cont nil))))))))


; run this at each load
(defun discuss-initialize nil
  (setq discuss-version
	"$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss.el,v 1.31 1991-07-30 20:06:59 lwvanels Exp $")

;;;
;;; Lots of autoload stuff....
;;;

(autoload 'discuss-talk (concat discuss-source-dir "/discuss-enter")
	  "Enter a new discuss transaction." t)

(autoload 'discuss-reply (concat discuss-source-dir "/discuss-enter")
	  "Reply to an existing discuss transaction." t)

(autoload 'discuss-randrp (concat discuss-source-dir "/discuss-enter")
	  "Random reply in a meeting." t)

(autoload 'discuss-ls (concat discuss-source-dir "/discuss-ls")
	  "List the headings of the transactions in a meeting." t)

(autoload 'discuss-list-acl (concat discuss-source-dir "/discuss-acl")
	  "List the ACL of a meeting." t)

(autoload 'discuss-forward (concat discuss-source-dir "/discuss-misc")
	  "Forward a transaction via mail." t)

(autoload 'discuss-reply-by-mail (concat discuss-source-dir "/discuss-misc")
	  "Forward a transaction via mail." t)

(autoload 'discuss-add-mtg (concat discuss-source-dir "/discuss-misc")
	  "Add a discuss meeting" t)

(autoload 'discuss-del-mtg (concat discuss-source-dir "/discuss-misc")
	  "Delete a discuss meeting" t)

;;; Keymaps, here at the end, where the trash belongs..
  
(if discuss-mtgs-mode-map
    nil
  (setq discuss-mtgs-mode-map (make-keymap))
  (suppress-keymap discuss-mtgs-mode-map)
  (define-key discuss-mtgs-mode-map "a" 'discuss-add-mtg)
  (define-key discuss-mtgs-mode-map "d" 'discuss-del-mtg)
  (define-key discuss-mtgs-mode-map "n" 'discuss-forward-meeting)
  (define-key discuss-mtgs-mode-map "p" 'discuss-prev-meeting)
  (define-key discuss-mtgs-mode-map " " 'discuss-next-meeting)
  (define-key discuss-mtgs-mode-map "\177" 'discuss-prev-meeting)
  (define-key discuss-mtgs-mode-map "l" 'discuss-lsm)
  (define-key discuss-mtgs-mode-map "g" 'discuss-goto)
  (define-key discuss-mtgs-mode-map "q" 'discuss-quit)
  (define-key discuss-mtgs-mode-map "s" 'discuss-stat)
  (define-key discuss-mtgs-mode-map "c" 'discuss-catchup)
  (define-key discuss-mtgs-mode-map "?" 'describe-mode))

(if discuss-trn-mode-map
    nil
  (setq discuss-trn-mode-map (make-keymap))
  (suppress-keymap discuss-trn-mode-map)
  (define-key discuss-trn-mode-map "." 'discuss-update)
  (define-key discuss-trn-mode-map " " 'scroll-up)
  (define-key discuss-trn-mode-map " " 'discuss-scroll-up)
  (define-key discuss-trn-mode-map "\177" 'scroll-down)
  (define-key discuss-trn-mode-map "n" 'discuss-next-trn)
  (define-key discuss-trn-mode-map "p" 'discuss-prev-trn)
  (define-key discuss-trn-mode-map "d" 'discuss-delete-trn)
  (define-key discuss-trn-mode-map "R" 'discuss-retrieve-trn)
  (define-key discuss-trn-mode-map "\en" 'discuss-nref)
  (define-key discuss-trn-mode-map "\ep" 'discuss-pref)
  (define-key discuss-trn-mode-map "g" 'discuss-show-trn)
  (define-key discuss-trn-mode-map "<" 'discuss-first-trn)
  (define-key discuss-trn-mode-map ">" 'discuss-last-trn)
  (define-key discuss-trn-mode-map "f" 'discuss-forward)
  (define-key discuss-trn-mode-map "F" 'discuss-toggle-trn-flag)
  (define-key discuss-trn-mode-map "h" 'discuss-trn-summary)
  (define-key discuss-trn-mode-map "\e\C-h" 'discuss-trn-summary)
  (define-key discuss-trn-mode-map "t" 'discuss-talk)
  (define-key discuss-trn-mode-map "r" 'discuss-reply)
  (define-key discuss-trn-mode-map "\C-c\C-r" 'discuss-randrp)
  (define-key discuss-trn-mode-map "\er" 'discuss-reply-by-mail)
  (define-key discuss-trn-mode-map "\C-o" 'discuss-trn-output)
  (define-key discuss-trn-mode-map "i" 'discuss-trn-input)
  (define-key discuss-trn-mode-map "q" 'discuss-leave-mtg)
  (define-key discuss-trn-mode-map "?" 'describe-mode)
  (define-key discuss-trn-mode-map "s" 'discuss-stat)
  (define-key discuss-trn-mode-map "a" 'discuss-add-mtg)
  (define-key discuss-trn-mode-map "\C-d" 'discuss-delete-trn-backwards)
  (define-key discuss-trn-mode-map "\ef" 'discuss-fref)
  (define-key discuss-trn-mode-map "\el" 'discuss-lref)
  (define-key discuss-trn-mode-map "=" 'discuss-ls)
  (define-key discuss-trn-mode-map "c" 'discuss-catchup)
  (define-key discuss-trn-mode-map "l" 'discuss-set-seen-and-leave-mtg)
)


  (fmakunbound 'discuss-initialize)
  )					;end of discuss-initialize
(discuss-initialize)

;;; discuss-trn-output mostly stolen from rmail-output...
;;; converted by [eichin:19881026.1505EST]
;(defvar discuss-output-last-file nil
;  "*Default file for discuss saves")

(defun discuss-trn-output (file-name)
  "Append this message to file named FILE-NAME."
  (interactive
   (list
    (read-file-name
     (concat "Append to: (default "
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

;;;
;;; this is just a quick hack, but I don't see an `better' way to do it...
;;;
(defun discuss-next-meeting (&optional quiet)
  "Find the next changed meeting in the discuss *meetings* buffer, or wrap."
  (interactive)
  (let ((buffer (current-buffer)))
    (set-buffer discuss-main-buffer)
    ;; If we're in DWIM mode, and we're currently looking at a changed
    ;; meeting, go to it.
    (if (and discuss-DWIM
	     (re-search-backward "^ c" (save-excursion (beginning-of-line)
						       (point))
				 t))
	(discuss-goto)
      (if (not (re-search-forward "^ c " nil t))
	  (progn
	    (goto-char (point-min))
	    (if (not (re-search-forward "^ c " nil t))
		(if (not quiet)
		    (message "No new meetings, try discuss-lsm instead."))
	      ))))
    (set-buffer buffer)))

(defun discuss-forward-meeting (&optional quiet)
  (interactive)
  (let ((discuss-DWIM nil))
    (discuss-next-meeting quiet)))

(defun discuss-prev-meeting ()
  "Find the next changed meeting in the discuss *meetings* buffer, or wrap."
  (interactive)
  (beginning-of-line)
  (if (not (re-search-backward "^ c " nil t))
      (progn
	(goto-char (point-max))
	(if (not (re-search-backward "^ c " nil t))
	    (message "No new meetings, try discuss-lsm instead.")
	  )))
  (forward-char 3))

(defun discuss-mark-read-meeting (meeting)
  "Mark a meeting as read on the discuss-mode listing."
  (save-excursion
    (set-buffer discuss-main-buffer)
    (goto-char (point-min))
    (if (not (re-search-forward (concat " " (regexp-quote meeting) 
					"\\(,\\|$\\)")
			     nil t))
	(progn
;	  (ding)
	  (message "Couldn't update changed flag for meeting %s." meeting))
      (progn
	(beginning-of-line)
	(forward-char 1)
	(let ((buffer-read-only nil))
	  (insert-char \032 1)
	  (delete-char 1))))))
