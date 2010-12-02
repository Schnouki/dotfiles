;; -----------------------------------------------------------------------------
;; Mail client
;; -----------------------------------------------------------------------------


;; Mail parameters
;(require 'sendmail)
;(require 'smtpmail)
;(require 'starttls)
;(require 'notmuch)

(setq user-full-name "Thomas Jost"
      user-mail-address "schnouki@schnouki.net"
      mail-smtp-address "mail.schnouki.net"
      message-directory "~/mail/"
      message-auto-save-directory nil)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "mail.schnouki.net"
      smtpmail-smtp-server "mail.schnouki.net"
      smtpmail-smtp-service 587
      smtpmail-debug-info nil
      smtpmail-debug-verb nil
      smtpmail-sendto-domain "schnouki.net"
      smtpmail-auth-credentials '(("mail.schnouki.net" 587 "schnouki" nil))
      smtpmail-starttls-credentials '(("mail.schnouki.net" 587 nil nil))
      starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments nil)

;; Load notmuch
(autoload 'notmuch "notmuch" nil t)

;; Global keys to launch notmuch
(global-set-key (kbd "C-! n") 'notmuch)

;; Various notmuch parameters:
;; - saved searches
;; - directory for sent messages
;; - kill message-mode buffer after a mail is sent
;; - poll script that fetches new mail
(setq notmuch-saved-searches '(("inbox"    . "tag:inbox")
			       ("unread"   . "tag:unread")
			       ("flagged"  . "tag:flagged")
			       ("todo"     . "tag:todo")
			       ("notes"    . "tag:notes")
			       ("drafts"   . "tag:draft")
			       ("sent"     . "tag:sent")
			       ("all MLs"  . "tag:ml and tag:unread")
			       ("april"    . "tag:april and tag:unread")
			       ("arch"     . "tag:arch and tag:unread")
			       ("awesome"  . "tag:awesome and tag:unread")
			       ("enigmail" . "tag:enigmail and tag:unread")
			       ("fsfe"     . "tag:fsfe and tag:unread")
			       ("notmuch"  . "tag:notmuch and tag:unread")
			       ("org-mode" . "tag:org-mode and tag:unread")
			       ("prosody"  . "tag:prosody and tag:unread")
			       ("pympress" . "tag:pympress and tag:unread")
			       ("social"   . "tag:social and tag:unread")
			       ("facebook" . "tag:facebook and tag:unread")
			       ("lwn"      . "from:lwn.net and tag:unread"))
      notmuch-fcc-dirs '(("thomas.jost@loria.fr" . "inria/Sent")
			 ("thomas.jost@inria.fr" . "inria/Sent")
			 (".*"                   . "schnouki.net/Sent"))
      message-kill-buffer-on-exit t
      notmuch-poll-script "~/.config/notmuch/mailsync")

;; Useful key bindings in notmuch buffers
(eval-after-load 'notmuch
  '(progn
     (defun notmuch-search-mark-read-and-archive-thread ()
       (interactive)
       (notmuch-search-remove-tag "inbox")
       (notmuch-search-remove-tag "unread")
       (forward-line))

     (defun notmuch-show-mark-read-and-archive-thread ()
       "Mark as read and archive each message in thread, then show next thread from search.

Mark as read and archive each message currently shown by removing
the \"unread\" and \"inbox\" tags from each. Then kill this
buffer and show the next thread from the search from which this
thread was originally shown.

Note: This command is safe from any race condition of new
messages being delivered to the same thread. It does not mark
read and archive the entire thread, but only the messages shown
in the current buffer."
       (interactive)
       (goto-char (point-min))
       (loop do (progn (notmuch-show-remove-tag "inbox")
		       (notmuch-show-remove-tag "unread"))
	     until (not (notmuch-show-goto-message-next)))
       ;; Move to the next item in the search results, if any.
       (let ((parent-buffer notmuch-show-parent-buffer))
	 (kill-this-buffer)
	 (if parent-buffer
	     (progn
	       (switch-to-buffer parent-buffer)
	       (forward-line)
	       (notmuch-search-show-thread)))))

     (defun notmuch-show-mark-read-and-archive-thread-then-exit ()
       "Mark read and archive each message in thread, then exit back to search results."
       (interactive)
       (notmuch-show-mark-read-and-archive-thread)
       (kill-this-buffer))

     (defun schnouki/notmuch-view-html ()
       "Open the HTML parts of a mail in a web browser."
       (interactive)
       (with-current-notmuch-show-message
	(let ((mm-handle (mm-dissect-buffer)))
	  (notmuch-foreach-mime-part
	   (lambda (p)
	     (if (string-equal (mm-handle-media-type p) "text/html")
		 (mm-display-external p (lambda ()
					  (message "Opening web browser...")
					  (browse-url-of-buffer)
					  (bury-buffer)))))
	   mm-handle))))

     (defun schnouki/notmuch-show-verify ()
       "Verify the PGP signature of the current mail."
       (interactive)
       (shell-command (concat "~/.config/notmuch/verify " (notmuch-show-get-filename)) "*Notmuch verify*"))

     (defun schnouki/notmuch-show-keys ()
       (interactive)
       (local-set-key "H" 'schnouki/notmuch-view-html)
       (local-set-key "W" 'schnouki/notmuch-show-verify)
       (local-set-key "z" 'notmuch-show-mark-read-and-archive-thread-then-exit))

     (defun schnouki/notmuch-search-keys ()
       (interactive)
       (local-set-key "z" 'notmuch-search-mark-read-and-archive-thread))

     (add-hook 'notmuch-show-hook 'schnouki/notmuch-show-keys)
     (add-hook 'notmuch-search-hook 'schnouki/notmuch-search-keys)

     ;; Temporary fix for the buggy Fcc handling
     (defun notmuch-fcc-header-setup ()
       "Add an Fcc header to the current message buffer.

Can be added to `message-send-hook' and will set the Fcc header
based on the values of `notmuch-fcc-dirs'. An existing Fcc header
will NOT be removed or replaced."
       
       (let ((subdir
	      (cond
	       ((or (not notmuch-fcc-dirs)
		    (message-fetch-field "Fcc"))
		;; Nothing set or an existing header.
		nil)
	       
	       ((stringp notmuch-fcc-dirs)
		notmuch-fcc-dirs)
	       
	       ((listp notmuch-fcc-dirs)
		(let* ((from (message-fetch-field "From"))
		       (match
			(catch 'first-match
			  (dolist (re-folder notmuch-fcc-dirs)
			    (when (string-match-p (car re-folder) from)
			      (throw 'first-match re-folder))))))
		  (if match
		      (cdr match)
		    (message "No Fcc header added.")
		    nil)))
	       
	       (t
		(error "Invalid `notmuch-fcc-dirs' setting (neither string nor list)")))))
	 
	 (when subdir
	   (message-add-header
	    (concat "Fcc: "
		    ;; If the resulting directory is not an absolute path,
		    ;; prepend the standard notmuch database path.
		    (if (= (elt subdir 0) ?/)
			subdir
		      (concat (notmuch-database-path) "/" subdir))))
	   
	   ;; finally test if fcc points to a valid maildir
	   (let ((fcc-header (message-fetch-field "Fcc")))
	     (unless (notmuch-maildir-fcc-dir-is-maildir-p fcc-header)
	       (cond ((not (file-writable-p fcc-header))
		      (error (format "No permission to create %s, which does not exist"
				     fcc-header)))
		     ((y-or-n-p (format "%s is not a maildir. Create it? "
					fcc-header))
		      (notmuch-maildir-fcc-create-maildir fcc-header))
		     (t
		      (error "Message not sent"))))))))

     ;; Display the hl-line correctly in notmuch-search
     (add-hook 'notmuch-search-hook '(lambda () (overlay-put global-hl-line-overlay 'priority 1)))

     ;; Choose signature according to the From header
     (defun schnouki/notmuch-choose-signature ()
       (let* ((from (message-fetch-field "From"))
	      (sigfile
	       (catch 'first-match
		 (dolist (re-file schnouki/message-signatures)
		   (when (string-match-p (car re-file) from)
		     (throw 'first-match (cdr re-file)))))))
	 (if sigfile
	     (with-temp-buffer
	       (insert-file-contents sigfile)
	       (buffer-string)))))

     (setq message-signature 'schnouki/notmuch-choose-signature
	   schnouki/message-signatures '(("thomas.jost@inria.fr" . "~/.signature/loria")
					 ("thomas.jost@loria.fr" . "~/.signature/loria")
					 (".*"                   . "~/.signature/schnouki")))))
