;;; 50-mail --- mail client
;;; Commentary:
;;; Code:

;; Mail parameters -- more of them in init-99-private.el ;)

;; Various notmuch parameters:
;; - saved searches
;; - kill message-mode buffer after a mail is sent
;; - poll script that fetches new mail
;; - addresses completion
;; - crypto stuff
(setq message-auto-save-directory nil
      send-mail-function 'message-send-mail-with-sendmail ;sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/home/schnouki/bin/msmtpq"
      message-sendmail-extra-arguments (list (concat "--domain=" (system-name)))
      message-send-mail-partially-limit nil
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      message-default-headers "X-Clacks-Overhead: GNU Terry Pratchett\n"
      gnus-inhibit-images nil
      notmuch-saved-searches '((:name "home"        :key "h" :query "(tag:inbox or tag:todo or tag:unread)")
			       (:name "unread"      :key "u" :query "tag:unread")
			       (:name "inbox"       :key "i" :query "tag:inbox")
			       (:name "blabla"      :key "b" :query "tag:blabla")
			       (:name "drafts"      :key "d" :query "tag:draft")
			       (:name "sent"        :key "s" :query "tag:sent")
			       (:name "flagged"     :key "f" :query "tag:flagged")
			       (:name "todo"        :key "t" :query "tag:todo")
			       (:name "spam"                 :query "tag:spam")

			       (:name "all MLs"     :key "m" :query "(tag:ml and tag:unread)")
			       (:name "d20"                  :query "(tag:d20 and tag:ml)")
			       (:name "april"                :query "(tag:april and tag:unread)")
			       (:name "arch"                 :query "(tag:arch and tag:unread)")
			       (:name "awesome"              :query "(tag:awesome and tag:unread)")
			       (:name "buddycloud"           :query "(tag:buddycloud and tag:unread)")
			       (:name "camlistore"           :query "(tag:camlistore and tag:unread)")
			       (:name "emacs"                :query "(tag:emacs and tag:unread)")
			       (:name "fsfe"                 :query "(tag:fsfe and tag:unread)")
			       (:name "ldn"                  :query "(tag:ldn and tag:unread)")
			       (:name "notmuch"              :query "(tag:notmuch and tag:unread)")
			       (:name "nybicc "              :query "(tag:nybi.cc and tag:unread)")
			       (:name "prosody"              :query "(tag:prosody and tag:unread)")
			       (:name "spop"                 :query "(tag:spop and tag:unread")
			       (:name "xmpp"                 :query "(tag:xmpp and tag:unread)"))
      notmuch-show-all-tags-list t
      notmuch-archive-tags '("-inbox" "-unread" "+archive")
      notmuch-address-command "~/.config/notmuch/addrbook.py"
      notmuch-crypto-process-mime t
      notmuch-search-oldest-first nil
      notmuch-mua-compose-in 'new-frame
      message-kill-buffer-on-exit t
      notmuch-print-mechanism 'notmuch-print-muttprint/evince
      notmuch-address-selection-function 'schnouki/notmuch-address-selection-function

      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "Le %e %B %Y à %-H:%M %Z, %N a écrit :"
      message-signature 'schnouki/choose-signature)

;; Add some features to message-mode
(add-hook 'message-setup-hook '(lambda () (footnote-mode t)))

;; Useful functions
(defun notmuch-search-filter-by-date (days)
  (interactive "NNumber of days to display: ")
  (let* ((now (current-time))
	 (beg (time-subtract now (days-to-time days)))
	 (filter
	  (concat
	   (format-time-string "%s.." beg)
	   (format-time-string "%s" now))))
    (notmuch-search-filter filter)))

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

(defun schnouki/notmuch-address-selection-function (prompt collection initial-input)
  (let ((completion-ignore-case t)
	(ido-enable-fle-matching t))
    (ido-completing-read
     prompt (cons initial-input collection) nil nil nil 'notmuch-address-history)))

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

(defun schnouki/notmuch-show-bounce (&optional address)
  "Bounce the current message."
  (interactive "sBounce To: ")
  (notmuch-show-view-raw-message)
  (message-resend address))

(defun schnouki/notmuch-show-edit-draft ()
  "Edit a draft message."
  (interactive)
  (unless (member "draft" (notmuch-show-get-tags))
    (error "This message is not tagged as draft"))
  (let ((filename (notmuch-show-get-filename)))
    ;; Prepare the mail
    (message-mail nil nil nil nil (notmuch-mua-get-switch-function))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (insert-file-contents filename)

    ;; Insert the separator if it's not there yet
    (goto-char (point-min))
    (unless (search-forward mail-header-separator nil t)
      (goto-char (point-min))
      (search-forward "\n\n")
      (forward-char -1)
      (insert mail-header-separator)
      (forward-line 1))

    ;; Update date and user agent
    (message-replace-header "Date" (message-make-date))
    (when notmuch-mua-user-agent-function
      (let ((user-agent (funcall notmuch-mua-user-agent-function)))
    	(when (not (string= "" user-agent))
    	  (message-replace-header "User-Agent" user-agent))))

    ;; Associate buffer with file name
    (setq buffer-file-name filename)
    (setq buffer-auto-save-file-name (make-auto-save-file-name))

    ;; Delete the file after the message is sent
    (add-to-list 'message-send-actions #'(lambda () (delete-file filename)))

    (message-sort-headers)
    (message-hide-headers)
    (set-buffer-modified-p nil)
    (notmuch-mua-maybe-set-window-dedicated)
    (message-goto-body)))

(defun schnouki/notmuch-signal-spamham (type &rest to)
  (with-current-notmuch-show-message
   (notmuch-show-forward-message)
   (message-replace-header "To" (mapconcat 'identity to ", "))
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

(defun schnouki/notmuch-search-show-thread-inhibit-images ()
  (interactive)
  (let ((gnus-inhibit-images t))
    (notmuch-search-show-thread)))

;; Display the hl-line correctly in notmuch-search
(defface schnouki/notmuch-hl-line '((t :inherit hl-line))
  "Face used for hl-line in notmuch-search mode")
(defun schnouki/notmuch-hl-line-mode ()
  (prog1 (hl-line-mode)
    (when hl-line-overlay
      (setq-local hl-line-face 'schnouki/notmuch-hl-line)
      (overlay-put hl-line-overlay 'priority 5))))
(add-hook 'notmuch-search-hook 'schnouki/notmuch-hl-line-mode)

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

;; Choose msmtp account used to send a mail according to the From header
;; schnouki/msmtp-accounts is a list cons cells: ("from_regexp" . "account").
(defun schnouki/change-msmtp-account ()
  "Change msmtp account according to the current From header."
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
(defun schnouki/notmuch-dbus-notify ()
  (save-excursion
    (save-restriction
      (when (get-buffer "*notmuch-hello*")
	(notmuch-hello-update t)))))

;; Load notmuch!
(use-package notmuch
  :bind (("C-! n" . notmuch)
	 ("C-! m" . notmuch-mua-new-mail))
  :config
  (progn
    ;; Show-mode keybindings
    (bind-keys :map notmuch-show-mode-map
	       ("b" . schnouki/notmuch-show-bounce)
	       ("e" . schnouki/notmuch-show-edit-draft)
	       ("H" . schnouki/notmuch-view-html)
	       ("r" . nil)
	       ("R" . nil)
	       ("ra" . notmuch-show-reply)
	       ("rs" . notmuch-show-reply-sender)
	       ("SH" . schnouki/notmuch-signal-ham)
	       ("SS" . schnouki/notmuch-signal-spam))

    ;; Search-mode keybindings
    (bind-keys :map notmuch-search-mode-map
	       ("d" . notmuch-search-filter-by-date)
	       ("C-<return>" . schnouki/notmuch-search-show-thread-inhibit-images))

    ;; Autorefresh notmuch-hello using D-Bus
    (require 'dbus)
    (ignore-errors
      (dbus-register-method :session dbus-service-emacs dbus-path-emacs
			    dbus-service-emacs "NotmuchNotify"
			    'schnouki/notmuch-dbus-notify))))

;; Use ido to read filename when attaching a file
(eval-after-load 'mml
  '(progn
     (defadvice mml-minibuffer-read-file (around ido-mml-minibuffer-read-file)
       (cl-flet ((read-file-name (prompt &optional dir default-fn mustmatch)
			      (ido-read-file-name prompt dir default-fn mustmatch)))
	 ad-do-it))
     (ad-activate 'mml-minibuffer-read-file)))

;; Don't try to display PDFs inline when they have a wrong MIME type
(eval-after-load 'mm-decode
  '(add-to-list 'mm-inline-media-tests '("text/pdf" ignore ignore)))

;; Viewers
(mailcap-add "image/.*" "geeqie %s" '(eq window-system 'x))

;; Other communication services :)
(use-package twittering-mode
  :ensure t
  :commands (twit twittering-mode))

;;; init-50-mail.el ends here
