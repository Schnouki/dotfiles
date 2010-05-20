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
      mail-signature t
      mail-signature-file "~/.signature/schnouki"
      message-signature t
      message-signature-file "~/.signature/schnouki"
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

;; Saved searches
(setq notmuch-saved-searches '(("inbox"    . "tag:inbox")
			       ("unread"   . "tag:unread")
			       ("flagged"  . "tag:flagged")
			       ("todo"     . "tag:todo")
			       ("drafts"   . "tag:draft")
			       ("sent"     . "tag:sent")
			       ("april"    . "tag:april and tag:unread")
			       ("arch"     . "tag:arch and tag:unread")
			       ("awesome"  . "tag:awesome and tag:unread")
			       ("fsfe"     . "tag:fsfe and tag:unread")
			       ("notmuch"  . "tag:notmuch and tag:unread")
			       ("org-mode" . "tag:org-mode and tag:unread")
			       ("pympress" . "tag:pympress and tag:unread")
			       ("social"   . "tag:social and tag:unread")
))

;; Directory for sent messages
(setq notmuch-fcc-dirs '(("Sent")))

;; Kill message-mode buffer after a mail is sent
(setq message-kill-buffer-on-exit t)

;; Useful key bindings in notmuch buffers
(eval-after-load 'notmuch
  '(progn
     (defun notmuch-search-mark-read-and-archive-thread ()
       (interactive)
       (notmuch-search-remove-tag "unread")
       (notmuch-search-archive-thread))

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
       "Open an HTML mail in a web browser."
       (interactive)
       (message "Opening in a web browser...")
       (notmuch-show-pipe-message "~/.config/notmuch/view-html"))

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

     (defun notmuch-search-color-line (start end line-tag-list)
       "Colorize lines in notmuch-search based on tags"
       (if notmuch-search-line-faces
	   (let ((overlay (make-overlay start end))
		 (tags-faces (copy-alist notmuch-search-line-faces))
		 line-face)
	     (while tags-faces
	       (let* ((tag-face (car tags-faces))
		      (tag (car tag-face))
		      (face (cdr tag-face)))
		 (if (member tag line-tag-list)
		     (setq line-face (append face line-face)))
		 (setq tags-faces (cdr tags-faces))))
	     (if line-face
		 (overlay-put overlay 'face line-face)))))

     ;; Display the hl-line correctly in notmuch-search
     (add-hook 'notmuch-search-hook '(lambda () (overlay-put global-hl-line-overlay 'priority 1)))))