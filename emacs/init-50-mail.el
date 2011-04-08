;; -----------------------------------------------------------------------------
;; Mail client
;; -----------------------------------------------------------------------------

;; Mail parameters -- more of them in init-99-private.el ;)
(setq message-auto-save-directory nil
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info nil
      smtpmail-debug-verb nil
      starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments nil)

;; Load notmuch
(autoload 'notmuch "notmuch" nil t)

;; Global keys to launch notmuch
(global-set-key (kbd "C-! n") 'notmuch)

;; Various notmuch parameters:
;; - saved searches
;; - kill message-mode buffer after a mail is sent
;; - poll script that fetches new mail
;; - addresses completion
;; - crypto stuff
(setq notmuch-saved-searches '(("inbox"    . "tag:inbox")
			       ("unread"   . "tag:unread")
			       ("flagged"  . "tag:flagged")
			       ("todo"     . "tag:todo")
			       ("notes"    . "tag:notes")
			       ("drafts"   . "tag:draft")
			       ("sent"     . "tag:sent")
			       ("d20"      . "tag:d20 and tag:ml")
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
      message-kill-buffer-on-exit t
      notmuch-poll-script "~/.config/notmuch/mailsync"
      notmuch-address-command "~/.config/notmuch/addrbook.py"
      notmuch-crypto-process-mime t)

;; Useful key bindings in notmuch buffers
(eval-after-load 'notmuch
  '(progn
     (defun notmuch-search-filter-by-date (days)
       (interactive "NNumber of days to display: ")
       (let* ((now (current-time))
	      (beg (time-subtract now (days-to-time days)))
	      (filter
	       (concat
		(format-time-string "%s.." beg)
		(format-time-string "%s" now))))
	 (notmuch-search-filter filter)))
     
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

     (defun schnouki/notmuch-hello-keys ()
       (interactive)
       (local-set-key "m" 'schnouki/notmuch-mua-mail))

     (defun schnouki/notmuch-show-keys ()
       (interactive)
       (local-set-key "H" 'schnouki/notmuch-view-html)
       (local-set-key "W" 'schnouki/notmuch-show-verify)
       (local-set-key "m" 'schnouki/notmuch-mua-mail)
       (local-set-key "z" 'notmuch-show-mark-read-and-archive-thread-then-exit))

     (defun schnouki/notmuch-search-keys ()
       (interactive)
       (local-set-key "d" 'notmuch-search-filter-by-date)
       (local-set-key "m" 'schnouki/notmuch-mua-mail)
       (local-set-key "z" 'notmuch-search-mark-read-and-archive-thread))

     (add-hook 'notmuch-show-hook 'schnouki/notmuch-show-keys)
     (add-hook 'notmuch-search-hook 'schnouki/notmuch-search-keys)

     ;; There's no notmuch-hello-hook...
     (defadvice notmuch-hello (after schnouki/notmuch-hello-set-keys)
       (schnouki/notmuch-hello-keys))
     (ad-activate 'notmuch-hello)

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

     ;; Custom version of notmuch address expansion. Just a little bit different.
     (defun notmuch-address-expand-name ()
       (ido-mode 1)
       (let* ((end (point))
	      (beg (save-excursion
		     (save-match-data
		       (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		       (match-end 0))))
	      (orig (buffer-substring-no-properties beg end))
	      (completion-ignore-case t)
	      (options (notmuch-address-options orig))
	      (num-options (length options))
	      (ido-enable-flex-matching t)
	      (chosen (if (eq num-options 1)
			  (car options)
			(ido-completing-read (format "Address (%s matches): " num-options)
					     options nil nil nil 'notmuch-address-history
					     (car options)))))
	 (when chosen
	   (push chosen notmuch-address-history)
	   (delete-region beg end)
	   (insert chosen))))

     ;; Display the hl-line correctly in notmuch-search
     (add-hook 'notmuch-search-hook '(lambda () (overlay-put global-hl-line-overlay 'priority 1)))))

;; Choose signature according to the From header
(defun schnouki/choose-signature ()
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
(setq message-signature 'schnouki/choose-signature)

;; Set From header according to the To header
(defun schnouki/choose-sender ()
  (let ((to (message-field-value "To")))
    (when to
      (let ((from
	     (catch 'first-match
	       (dolist (rule schnouki/message-sender-rules)
		 (when (string-match-p (car rule) to)
		   (throw 'first-match (cdr rule)))))))
	(if from
	    (progn
	      (setq from (concat user-full-name " <" from ">"))
	      (message-replace-header "From" from)
	      (message (concat "Sender set to " from))))))))
(add-hook 'message-setup-hook 'schnouki/choose-sender)

;; Choose the identify used to write a new mail
(defun schnouki/notmuch-mua-mail (&optional from)
  (interactive)
  (unless from
    (setq from (ido-completing-read "Sender identity: " schnouki/mua-identities
				    nil nil nil nil (car schnouki/mua-identities))))
  (notmuch-mua-mail nil nil (list (cons 'from from))))

