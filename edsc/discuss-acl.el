;;;
;;;	Copyright (C) 1990 by the Massachusetts Institute of Technology
;;;    	Developed by the MIT Student Information Processing Board (SIPB).
;;;    	For copying information, see the file mit-copyright.h in this release.
;;;
;;;	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-acl.el,v $
;;;	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/discuss-acl.el,v 1.2 1996-04-12 21:44:09 ghudson Exp $
;;;
;;;  Emacs lisp code to deal with ACL manipulation in discuss meetings
;;;  Written by Theodore Ts'o
;;;

(defvar discuss-acl-mode-map nil
  "Keymap used by the acl mode of the discuss subsystem")

(defvar discuss-acl-filename nil
  "Filename used to store the ACL information.")

;;; Discuss acl mode

(defun discuss-acl-mode ()
"Major mode for looking at the ACL (Access Control List)  of a meeting under
the discuss subsystem.
All normal editing commands are turned off.
Instead, these commands are available:
\\[describe-mode]	List available commands.
\\[scroll-up]	Scroll to next screen of this transaction.
\\[scroll-down]	Scroll to previous screen of this transaction.
\\[discuss-acl-quit]	Quit looking at the ACL"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'discuss-acl-mode)
  (setq mode-name "Discuss (acl)")
  (use-local-map discuss-acl-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'discuss-acl-hooks))


(defun discuss-list-acl ()
  "Display the ACL of the meeting"
  (interactive)
  (if (and discuss-acl-filename
	   (file-exists-p discuss-acl-filename))
      (delete-file discuss-acl-filename))
  (setq discuss-acl-filename (make-temp-name (format "/tmp/discuss-acl-%s" 
						     discuss-current-meeting)))
  (discuss-send-cmd (format "(pacl %s %s)\n"
			    discuss-acl-filename
			    discuss-current-meeting)
		    'discuss-end-list-acl 'discuss-read-form))

(defun discuss-end-list-acl ()
  (message "done list acl")
  (setq discuss-acl-buf
	(get-buffer-create "*discuss-acl*"))
  (let ((pop-up-windows t))
    (pop-to-buffer discuss-acl-buf))
  (discuss-acl-mode)
  (let ((buffer-read-only nil))
    (insert-file-contents discuss-acl-filename))
  )

(defun discuss-acl-quit ()
  (interactive)
  (if (and discuss-acl-filename
	   (file-exists-p discuss-acl-filename))
      (delete-file discuss-acl-filename))
  (setq discuss-acl-filename nil)
  (if discuss-current-meeting
      (switch-to-buffer discuss-cur-mtg-buf))
  (delete-other-windows)
  (if (and discuss-acl-buf
	   (buffer-name discuss-acl-buf))
      (kill-buffer discuss-acl-buf))
  (setq discuss-acl-buf nil))

(if discuss-acl-mode-map
    nil
  (setq discuss-acl-mode-map (make-keymap))
  (suppress-keymap discuss-acl-mode-map)
  (define-key discuss-acl-mode-map "?" 'describe-mode)
  (define-key discuss-acl-mode-map " " 'scroll-up)
  (define-key discuss-acl-mode-map "\177" 'scroll-down)
  (define-key discuss-acl-mode-map "q" 'discuss-acl-quit)
)

