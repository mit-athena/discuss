;;;	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-enter.el,v $
;;;	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-enter.el,v 1.5 1988-11-01 15:57:21 eichin Exp $
;;;
;;;  Emacs lisp code to enter transaction into discuss.  Part of the
;;;  emacs-based interface to the discuss conferencing system.
;;;
;;;  Copyright (c) 1988 by the Massachusetts Institute of Technology.
;;;  Written by Stan Zanarotti and Bill Sommerfeld.
;;;

(require 'discuss)

(defvar discuss-new-trn-buf nil
  "Name of buffer for new transaction")

(defvar discuss-trn-buffer "*discuss-unprocessed*"
  "Name of buffer for entering new transaction")

(defvar discuss-edit-mode-map nil
  "Keymap used by the edit mode of the discuss subsystem")

(defvar discuss-randrp-trn-info nil
  "Randrom transaction information.")

(defun discuss-temp-file ()
  "Return name of temporary buffer to use to transfer transactions."
  (format "/tmp/em.disc%d" 
	  (process-id (get-buffer-process discuss-shell-buffer))))

(defun discuss-talk ()
  "Enter a new discuss transaction."
  (interactive)
  (discuss-enter discuss-current-meeting 0 ""))

(defun discuss-reply ()
  "Reply to an existing discuss transaction."
  (interactive)
  (let ((subject (nth 11 discuss-current-transaction-info)))
    ;; Add the Re: field
    (if (and (> (length subject) 3)
	     (string-match "[Rr]e: " (substring subject 0 4)))
	nil
      (setq subject (concat "Re: " subject)))
    (discuss-enter discuss-current-meeting discuss-current-transaction
		   subject t)))

(defun discuss-randrp ()
  "Randrp in a meeting."
  (interactive)
  (discuss-send-cmd (format "(grtn %s)\n" discuss-current-meeting)
		    'discuss-randrp-gmi-end 'discuss-read-form))

(defun discuss-randrp-gmi-end () 
  (setq discuss-randrp-trn-info discuss-form)
  (let ((subject (nth 11 discuss-randrp-trn-info))
	(trn-num (nth 0 discuss-randrp-trn-info)))
    (if (and (> (length subject) 3)
	     (string-match "[Rr]e: " (substring subject 0 4)))
	nil
      (setq subject (concat "Re: " subject)))
    (discuss-enter discuss-current-meeting trn-num subject t)))

(defun discuss-enter (mtg-name reply-trn subject &optional reply)
    (setq discuss-new-trn-buf
	  (get-buffer discuss-trn-buffer))
    (if discuss-new-trn-buf
	(error "Already entering transaction")
      (progn
	(setq discuss-new-trn-buf
	      (get-buffer-create discuss-trn-buffer))
	(if (not (null reply))
	    (let ((pop-up-windows t))
	      (pop-to-buffer discuss-new-trn-buf))
	  (switch-to-buffer discuss-new-trn-buf))
	(discuss-edit-mode)
	(insert "Subject: "
		subject
		"\n"
		mail-header-separator
		"\n")
	(setq discuss-reply-trn reply-trn)
	(setq discuss-enter-mtg mtg-name)
	(if (equal subject "")
	    (progn
	      (goto-char (point-min))
	      (end-of-line))))))

(defun discuss-edit-mode ()
  "Major mode for editing Discuss transactions to be entered.
Like Text Mode but with these additional commands:
C-c C-c  discuss-send (enter the transaction)
C-c C-]  discuss-abort-edit (exit without enterring)"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'discuss-reply-trn)
  (setq discuss-reply-trn 0)
  (make-local-variable 'discuss-enter-mtg)
  (setq discuss-enter-mtg nil)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map discuss-edit-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'discuss-edit-mode)
  (setq mode-name "Discuss Edit")
  (setq buffer-offer-save t)
  (run-hooks 'text-mode-hook 'discuss-edit-mode-hook))

(if discuss-edit-mode-map
    nil
  (setq discuss-edit-mode-map (make-sparse-keymap))
  (define-key discuss-edit-mode-map "\C-c?" 'describe-mode)
  (define-key discuss-edit-mode-map "\C-c\C-c" 'discuss-send)
  (define-key discuss-edit-mode-map "\C-c\C-]" 'discuss-abort-edit))


(defun discuss-send ()
  "Enter the current Discuss transaction"
  (interactive)
  (save-excursion
    ;; Make sure buffer ends with a newline.
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    ;; get subject, and find separator
    (goto-char (point-min))
    (if (not (looking-at "Subject: "))
	(error "Subject not found.")
      (forward-line 1)
      (let ((subject (buffer-substring 10 (1- (point)))))
	(if (not (re-search-forward
		  (concat "^" (regexp-quote mail-header-separator) "\n")
		  (point-max)
		  t))
	    (error "Text separator not found.")
	  (write-region (point) (point-max) (discuss-temp-file))
	  (message "Sending...")
	  (discuss-send-cmd (format "(at %d %s %s)\n%s\n"
				    discuss-reply-trn
				    (discuss-temp-file)
				    discuss-enter-mtg
				    subject)
			    'discuss-end-of-enter 'discuss-read-form)
	  )))))

(defun discuss-end-of-enter ()
  (set-buffer discuss-trn-buffer)
  (message (format "Transaction %s entered in %s meeting."
		   (discuss-format-trn-num (car discuss-form))
		   discuss-enter-mtg))
  (if (not (one-window-p)) (delete-window))
  (kill-buffer discuss-trn-buffer)
  (discuss-update))

(defun discuss-abort-edit ()
  "Aborts entering a transaction."
  (interactive)
  (if (not (one-window-p)) (delete-window))
  (kill-buffer discuss-trn-buffer))
