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
      starttls-extra-arguments nil
      mail-specify-envelope-from t
      mail-envelope-from 'header)

;; Load notmuch
(add-to-list 'load-path "~/dev/notmuch/emacs")
(autoload 'notmuch "notmuch" nil t)

;; Global keys to launch notmuch
(global-set-key (kbd "C-! n") 'notmuch)

;; Various notmuch parameters:
;; - saved searches
;; - kill message-mode buffer after a mail is sent
;; - poll script that fetches new mail
;; - addresses completion
;; - crypto stuff
(setq notmuch-saved-searches '(("inbox"       . "tag:inbox")
			       ("unread"      . "tag:unread")
			       ("flagged"     . "tag:flagged")
			       ("todo"        . "tag:todo")
			       ("notes"       . "tag:notes")
			       ("drafts"      . "tag:draft")
			       ("sent"        . "tag:sent")
			       ("d20"         . "tag:d20 and tag:ml")
			       ("kléber"      . "tag:kléber")
			       ("mp2"         . "tag:mp2")
			       ("inria"       . "folder:inria and tag:unread")
			       ("all MLs"     . "tag:ml and tag:unread")
			       ("april"       . "tag:april and tag:unread")
			       ("arch"        . "tag:arch and tag:unread")
			       ("awesome"     . "tag:awesome and tag:unread")
			       ("freedombox"  . "tag:freedombox and tag:unread")
			       ("fsfe"        . "tag:fsfe and tag:unread")
			       ("ldn"         . "tag:ldn and tag:unread")
			       ("notmuch"     . "tag:notmuch and tag:unread")
			       ("nybicc "     . "tag:hackerspace and tag:unread")
			       ("offlineimap" . "tag:offlineimap and tag:unread")
			       ("org-mode"    . "tag:org-mode and tag:unread")
			       ("prosody"     . "tag:prosody and tag:unread")
			       ("pympress"    . "tag:pympress and tag:unread")
			       ("social"      . "tag:social and tag:unread")
			       ("facebook"    . "tag:facebook and tag:unread")
			       ("lwn"         . "from:lwn.net and tag:unread"))
      message-kill-buffer-on-exit t
      notmuch-poll-script "~/.config/notmuch/mailsync"
      notmuch-address-command "~/.config/notmuch/addrbook.py"
      notmuch-crypto-process-mime t)

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
       (local-set-key "d" 'notmuch-search-filter-by-date)
       (local-set-key "z" 'notmuch-search-mark-read-and-archive-thread))

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

;; Choose SMTP server used to send a mail according to the From header
;; Inspired by http://www.emacswiki.org/emacs/MultipleSMTPAccounts
;; schnouki/smtp-servers is a list of lists. Each of these lists has 4 elements:
;; corresponding sender address, server hostname, server port, server domain.
(defun schnouki/change-smtp ()
  "Change SMTP server according to the current From header"
  (let* ((from (downcase (cadr (mail-extract-address-components (message-field-value "From")))))
	 (server (assoc from schnouki/smtp-servers)))
    (when server
      (make-local-variable 'smtpmail-smtp-server)
      (make-local-variable 'smtpmail-smtp-service)
      (make-local-variable 'smtpmail-sendto-domain)
      (setq smtpmail-smtp-server   (nth 1 server)
	    smtpmail-smtp-service  (nth 2 server)
	    smtpmail-sendto-domain (nth 3 server)))))
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
       (when (get-buffer "*notmuch-hello*")
	 (message "Notmuch notify")
	 (notmuch-hello-update t)))
     (dbus-register-method :session dbus-service-emacs dbus-path-emacs
			   dbus-service-emacs "NotmuchNotify"
			   'schnouki/notmuch-dbus-notify)))

;; Turn "id:..." to links to notmuch searches in notmuch-show
(eval-after-load 'notmuch
  '(progn
     (require 'thingatpt)
     (when (featurep 'goto-addr)
       (unload-feature 'goto-addr))

     (defvar notmuch-show-mail-regexp
       "[-a-zA-Z0-9=._+]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+"
       "A regular expression probably matching an e-mail address.")

     (defvar notmuch-show-url-regexp
       (concat
	"\\<\\("
	(mapconcat 'identity
		   (delete "mailto:"
			   (copy-sequence thing-at-point-uri-schemes))
		   "\\|")
	"\\)"
	thing-at-point-url-path-regexp)
       "A regular expression probably matching a URL.")

     (defvar notmuch-show-search-regexp
       (concat
	"\\<\\("
	(mapconcat 'identity
		   '("id:\".+?\""
		     "tag:[-a-zA-Z0-9._+]+"
		     "thread:[a-fA-F0-9]+")
		   "\\|")
	"\\)")
       "A regular expression probably matching a notmuch search.")

     (defcustom notmuch-show-mail-face 'italic
       "Face to use for e-mail addresses."
       :type 'face
       :group 'notmuch)

     (defcustom notmuch-show-mail-mouse-face 'secondary-selection
       "Face to use for e-mail addresses when the mouse is on them."
       :type 'face
       :group 'notmuch)

     (defcustom notmuch-show-url-face 'link
       "Face to use for URLs."
       :type 'face
       :group 'notmuch)

     (defcustom notmuch-show-url-mouse-face 'highlight
       "Face to use for URLs when the mouse is on them."
       :type 'face
       :group 'notmuch)

     (defcustom notmuch-show-search-face 'italic
       "Face to use for notmuch search links."
       :type 'face
       :group 'notmuch)

     (defcustom notmuch-show-search-mouse-face 'secondary-selection
       "Face to use for notmuch search links when the mouse is on them."
       :type 'face
       :group 'notmuch)

     (define-button-type 'notmuch-show-mail-button-type
       'action 'notmuch-show-mail-button-action
       'follow-link t
       'help-echo "Mail this address."
       'face 'notmuch-show-mail-face
       'mouse-face 'notmuch-show-mail-mouse-face)

     (define-button-type 'notmuch-show-url-button-type
       'action 'notmuch-show-url-button-action
       'follow-link t
       'help-echo "Browse this URL."
       'face 'notmuch-show-url-face
       'mouse-face 'notmuch-show-url-mouse-face)

     (define-button-type 'notmuch-show-search-button-type
       'action 'notmuch-show-search-button-action
       'follow-link t
       'help-echo "Open in notmuch."
       'face 'notmuch-show-search-face
       'mouse-face 'notmuch-show-search-mouse-face)

     (defun notmuch-show-mail-button-action (button)
       (let ((to (button-get button :notmuch-link)))
	 (notmuch-mua-new-mail nil to)))

     (defun notmuch-show-url-button-action (button)
       (let ((url (button-get button :notmuch-link)))
	 (browse-url url)))

     (defun notmuch-show-search-button-action (button)
       (let ((query (button-get button :notmuch-link)))
	 (notmuch-search query)))

     (defun notmuch-show-buttonize ()
       (goto-address-mode nil)
       (save-excursion
	 (let ((inhibit-point-motion-hooks t)
	       (inhibit-read-only t))
	   (goto-char (point-min))
	   (while (re-search-forward notmuch-show-search-regexp nil t)
	     (let ((s (match-beginning 0))
		   (e (match-end 0))
		   (query (match-string 0)))
	       (unless (button-at s)
		 (make-button s e
			      :type 'notmuch-show-search-button-type
			      :notmuch-link query))))
	   (goto-char (point-min))
	   (while (re-search-forward notmuch-show-mail-regexp nil t)
	     (let ((s (match-beginning 0))
		   (e (match-end 0))
		   (to (match-string 0)))
	       (unless (button-at s)
		 (make-button s e
			      :type 'notmuch-show-mail-button-type
			      :notmuch-link to))))
	   (goto-char (point-min))
	   (while (re-search-forward notmuch-show-url-regexp nil t)
	     (let ((s (match-beginning 0))
		   (e (match-end 0))
		   (url (match-string 0)))
	       (unless (button-at s)
		 (make-button s e
			      :type 'notmuch-show-url-button-type
			      :notmuch-link url)))))))
     (add-hook 'notmuch-show-hook 'notmuch-show-buttonize)))
