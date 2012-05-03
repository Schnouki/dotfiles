;; -----------------------------------------------------------------------------
;; Mail client
;; -----------------------------------------------------------------------------

;; Mail parameters -- more of them in init-99-private.el ;)
(setq message-auto-save-directory nil
      send-mail-function 'message-send-mail-with-sendmail ;sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      message-sendmail-extra-arguments (list (concat "--domain=" (system-name)))
      message-send-mail-partially-limit nil
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; Load notmuch
(add-to-list 'load-path "~/dev/notmuch/emacs")
(autoload 'notmuch "notmuch" nil t)
(autoload 'notmuch-mua-new-mail "notmuch" nil t)

;; Global keys to launch notmuch
(global-set-key (kbd "C-! n") 'notmuch)
(global-set-key (kbd "C-ç n") 'notmuch)
(global-set-key (kbd "C-! m") 'notmuch-mua-new-mail)
(global-set-key (kbd "C-ç m") 'notmuch-mua-new-mail)

;; Various notmuch parameters:
;; - saved searches
;; - kill message-mode buffer after a mail is sent
;; - poll script that fetches new mail
;; - addresses completion
;; - crypto stuff
(setq notmuch-saved-searches '(("home"        . "(tag:inbox or tag:todo or tag:unread)")
			       ("unread"      . "tag:unread")
			       ("inbox"       . "tag:inbox")
			       ("flagged"     . "tag:flagged")
			       ("todo"        . "tag:todo")
			       ("notes"       . "tag:notes")
			       ("drafts"      . "tag:draft")
			       ("sent"        . "tag:sent")
			       ("d20"         . "(tag:d20 and tag:ml)")
			       ("kléber"      . "tag:kléber")
			       ("mp2"         . "tag:mp2")
			       ("Gmail"       . "(folder:Gmail and tag:unread)")
			       ("inria"       . "(folder:inria and tag:unread)")
			       ("all MLs"     . "(tag:ml and tag:unread)")
			       ("april"       . "(tag:april and tag:unread)")
			       ("arch"        . "(tag:arch and tag:unread)")
			       ("awesome"     . "(tag:awesome and tag:unread)")
			       ("freedombox"  . "(tag:freedombox and tag:unread)")
			       ("fsfe"        . "(tag:fsfe and tag:unread)")
			       ("ldn"         . "(tag:ldn and tag:unread)")
			       ("notmuch"     . "(tag:notmuch and tag:unread)")
			       ("nybicc "     . "(tag:hackerspace and tag:unread)")
			       ("offlineimap" . "(tag:offlineimap and tag:unread)")
			       ("org-mode"    . "(tag:org-mode and tag:unread)")
			       ("prosody"     . "(tag:prosody and tag:unread)")
			       ("pympress"    . "(tag:pympress and tag:unread)")
			       ("social"      . "(tag:social and tag:unread)")
			       ("facebook"    . "(tag:facebook and tag:unread)")
			       ("lwn"         . "(from:lwn.net and tag:unread)"))
      notmuch-poll-script "~/.config/notmuch/mailsync"
      notmuch-address-command "~/.config/notmuch/addrbook.py"
      notmuch-crypto-process-mime t
      notmuch-mua-compose-in 'new-frame
      message-kill-buffer-on-exit t
      notmuch-thousands-separator " "

      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "Le %e %B %Y à %-H:%M %Z, %N a écrit :")

;; Add some features to message-mode
(add-hook 'message-setup-hook '(lambda () (footnote-mode t)))

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

     (defun schnouki/notmuch-search-archive-thread ()
       (interactive)
       (notmuch-search-tag '("-inbox" "-unread"))
       (notmuch-search-next-thread))

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
       (local-unset-key "r")
       (local-unset-key "R")
       (local-set-key "ra" 'notmuch-show-reply)
       (local-set-key "rs" 'notmuch-show-reply-sender)
       (local-set-key "SH" 'schnouki/notmuch-signal-ham)
       (local-set-key "SS" 'schnouki/notmuch-signal-spam)
       (local-set-key "W" 'schnouki/notmuch-show-verify))

     (defun schnouki/notmuch-search-keys ()
       (interactive)
       (local-set-key "a" 'schnouki/notmuch-search-archive-thread)
       (local-set-key "d" 'notmuch-search-filter-by-date))

     (add-hook 'notmuch-show-hook 'schnouki/notmuch-show-keys)
     (add-hook 'notmuch-search-hook 'schnouki/notmuch-search-keys)

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

     (defun notmuch-mua-mail-url (url)
       (interactive (browse-url-interactive-arg "Mailto URL: "))
       (let* ((alist (rfc2368-parse-mailto-url url))
	      (to (assoc "To" alist))
	      (subject (assoc "Subject" alist))
	      (body (assoc "Body" alist))
	      (rest (delete to (delete subject (delete body alist))))
	      (to (cdr to))
	      (subject (cdr subject))
	      (body (cdr body))
	      (mail-citation-hook (unless body mail-citation-hook)))
	 (notmuch-mua-mail to subject rest nil nil
			   (if body
			       (list 'insert body)
			     (list 'insert-buffer (current-buffer))))))

     (defun schnouki/notmuch-signal-spamham (type)
       (with-current-notmuch-show-message
	(notmuch-mua-forward-message)
	(message-replace-header "To" (concat "sa-" type "@schnouki.net"))
	(message-remove-header "Fcc")
	(message-sort-headers)
	(message-hide-headers)
	(message-goto-to)
	(set-buffer-modified-p nil)
	(if (yes-or-no-p (concat "Really flag this as " type "?"))
	    (message-send-and-exit)
	  (progn
	    (message-kill-buffer)
	    (delete-frame)))))

     (defun schnouki/notmuch-signal-spam ()
       (interactive)
       (schnouki/notmuch-signal-spamham "spam"))

     (defun schnouki/notmuch-signal-ham ()
       (interactive)
       (schnouki/notmuch-signal-spamham "ham"))

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
;; schnouki/message-sender-rules is a list of cons cells: if the "To" header
;; matched the car of an entry, then From is set to the cdr of that entry.
;; e.g. '(("@gmail.com" . "me@gmail.com")
;;        ("some-ml@whatever.com" . "subscribed-address@eggbaconandspam.com"))
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

;; TODO: check message-alternative-emails...

;; Choose msmtp account used to send a mail according to the From header
;; schnouki/msmtp-accounts is a list cons cells: ("from_regexp" . "account").
(defun schnouki/change-msmtp-account ()
  "Change msmtp account according to the current From header"
  (let* ((from (downcase (cadr (mail-extract-address-components (message-field-value "From")))))
	 (account
	  (catch 'first-match
	    (dolist (re-account schnouki/msmtp-accounts)
	      (when (string-match-p (car re-account) from)
		(throw 'first-match (cdr re-account)))))))
    (make-local-variable 'message-sendmail-extra-arguments)
    (if account
	(setq message-sendmail-extra-arguments (append (list "-a" account) message-sendmail-extra-arguments))
      (error (concat "Sender address does not match any msmtp account: " account)))))
(add-hook 'message-send-mail-hook 'schnouki/change-msmtp-account)

(defadvice smtpmail-via-smtp (before schnouki/set-smtp-account
 				     (&optional recipient smtpmail-text-buffer))
   "First set SMTP account."
     (with-current-buffer smtpmail-text-buffer (schnouki/change-smtp)))
(ad-activate 'smtpmail-via-smtp)

;; Autorefresh notmuch-hello using D-Bus
(eval-after-load 'notmuch
  '(progn
     (require 'dbus)
     (defun schnouki/notmuch-dbus-notify ()
       (save-excursion
	 (save-restriction
	   (when (get-buffer "*notmuch-hello*")
	     (notmuch-hello-update t)))))
     (ignore-errors
       (dbus-register-method :session dbus-service-emacs dbus-path-emacs
			     dbus-service-emacs "NotmuchNotify"
			     'schnouki/notmuch-dbus-notify))))

;; Use ido to read filename when attaching a file
(eval-after-load 'mml
  '(progn
     (defadvice mml-minibuffer-read-file (around ido-mml-minibuffer-read-file)
       (flet ((read-file-name (prompt &optional dir default-fn mustmatch)
			      (ido-read-file-name prompt dir default-fn mustmatch)))
	 ad-do-it))
     (ad-activate 'mml-minibuffer-read-file)))
