;;; 20-utils --- Small useful functions and key bindings
;;; Commentary:
;;; Code:

;; bind-key
(use-package bind-key
  :ensure t)

;; Set justification with C-x M-f
(bind-key "C-x M-f" 'set-justification)

;; Tweak visual-line stuff
(setq line-move-visual nil)
(bind-key "C-x t" 'toggle-truncate-lines)

;; Auto-update buffers when the file changes on-disk
(global-auto-revert-mode 1)
(bind-key "C-x r RET" 'revert-buffer)

;; Copy current line with M-k
;; http://www.emacsblog.org/2009/05/18/copying-lines-not-killing/#comment-27462
(defun schnouki/copy-line ()
  "Copy the current line to the kill-ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (+ 1 (line-end-position))))
(bind-key "M-k" 'schnouki/copy-line)

;; Switch to scratch buffer, creating it if necessary
;; http://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs/776052#776052
(defun schnouki/goto-scratch (&optional force-new)
  "Switch to scratch buffer, creating it if necessary.
Calling this function with a prefix FORCE-NEW forces the creation of a new buffer."
  (interactive "P")
  (let ((sb (if force-new
	       (generate-new-buffer "*scratch*")
	     (get-buffer-create "*scratch*"))))
    (switch-to-buffer sb)
    (lisp-interaction-mode)))
(bind-key "C-x M-s" 'schnouki/goto-scratch)

;; ibuffer
(bind-key "C-x C-b" 'ibuffer)

;; "Smart" home key
;; Beginning of indented text --> beginning of "real" text --> beginning of line
(defun schnouki/home-key ()
  "'Smart' home key manager."
  (interactive "^")
  (let
      ((pos-current (current-column))                                ;; Current position
       (pos-indent (progn (back-to-indentation) (current-column)))   ;; Beginning of indented text
       (pos-real (progn (beginning-of-line-text) (current-column)))) ;; Beginning of real text

    ;; If at beginning of the indented text and if it's not the same as real
    ;; text, go to real text
    (if (and (= pos-current pos-indent) (not (= pos-indent pos-real)))
	(move-to-column pos-real)
      ;; Else, if at beginning of real text, go to beginning of line
      (if (= pos-current pos-real) (move-to-column 0)
	;; Else, go to beginning of indented text
	(move-to-column pos-indent)))))
(global-set-key [home] 'schnouki/home-key)

;; Insert newline and return to point
(defun schnouki/newline-same-point ()
  "Insert newline and return to point."
  (interactive)
  (save-excursion
    (newline-and-indent))
  (funcall indent-line-function))
(bind-key "M-RET" 'schnouki/newline-same-point)

;; Quick diff between current buffer and file
;; From http://slashusr.wordpress.com/2010/01/19/quickly-diff-the-changes-made-in-the-current-buffer-with-its-file/
(defun schnouki/diff-current-buffer-with-file ()
  "Quick diff between current buffer and a file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(bind-key "C-x =" 'schnouki/diff-current-buffer-with-file)

;; Enlarge/shrink window horozontally/vertically
(bind-keys ("C-M-j" . shrink-window)
	   ("C-M-k" . enlarge-window)
	   ("C-M-h" . shrink-window-horizontally)
	   ("C-M-l" . enlarge-window-horizontally))

;; Convert seconds to a duration
(defun schnouki/seconds-to-duration (seconds)
  "Convert seconds to a readable duration."
  (interactive)
  (let* ((secs (if (numberp seconds) seconds
		(if (stringp seconds) (string-to-number seconds)
		  (error "Argument must be a number or a string"))))
	 (h (floor secs 3600))
	 (m (floor (mod secs 3600) 60))
	 (s (floor (mod secs 60))))
    (concat
     (if (> h 0) (concat (number-to-string h) "h" (if (or (> m 0) (> s 0)) " ")))
     (if (> m 0) (concat (number-to-string m) "m" (if (> s 0) " ")))
     (if (> s 0) (concat (number-to-string s) "s")))))

;; Position function for strings
(defun string-position (item seq)
  "Find the first occurence of ITEM in SEQ.
Return the index of the matching item, or nil if not found."
  (let ((len (length seq))
	(count 0))
    (while (and (< count len) (not (string= item (nth count seq))))
      (setq count (1+ count)))
    (if (= count len) nil count)))

;; Remove *blabla* buffers, except those that match a regexp in the
;; immortal-star-buffers list or a major mode in the immortal-modes list.
(defvar schnouki/immortal-star-buffers nil)
(defvar schnouki/immortal-modes nil)
(setq schnouki/immortal-star-buffers '("^\\*scratch\\*")
      schnouki/immortal-modes        '(message-mode notmuch-hello-mode notmuch-search-mode
				       notmuch-show-mode org-agenda-mode inferior-python-mode
				       jabber-chat-mode jabber-roster-mode))
(defun schnouki/kill-star-buffers (&optional arg)
  "Remove most star-buffers (`*Messages*', `*Compilation', ...) that are not in the `schnouki/immortal-star-buffers' list.  With prefix argument ARG, kill all star-buffers."
  (interactive "P")
  (let ((count 0)
	buf-name buf-mode)
    (dolist (buf (buffer-list))
      (setq buf-name (buffer-name buf)
	    buf-mode (with-current-buffer buf major-mode))
      (when (and
	     (string-match "^\\*.+$" buf-name)
	     (or arg
		 (and (notany '(lambda (re) (string-match re buf-name)) schnouki/immortal-star-buffers)
		      (not (memq buf-mode schnouki/immortal-modes)))))
	(kill-buffer buf)
	(setq count (1+ count))))
    (message (concat (int-to-string count) " buffers killed"))))
(bind-key "C-x M-k" 'schnouki/kill-star-buffers)

;; ido-mode for better buffer switching, file selection, etc.
(use-package ido
  :config
  (progn
    (setq ido-default-file-method 'selected-window
	  ido-default-buffer-method 'selected-window)))
(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (ido-vertical-mode 1)))

;; ack
(use-package ack-and-a-half
  :ensure t
  :bind (("C-! M-<" . ack-and-a-half)
	 ("C-! <"   . ack-and-a-half-same)
	 ("C-! M-f" . ack-and-a-half-find-file)
	 ("C-! f"   . ack-and-a-half-find-file-same))
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (progn
    ;; Lighter displayed in mode line
    (setq undo-tree-mode-lighter nil)
    ;; ...and enable!
    (global-undo-tree-mode)))

;; avy (reminder: C-x C-SPC to pop-global-mark)
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-word-or-subword-1)
	 ("C-x C-;" . avy-pop-mark))
  :config
  (progn
    (avy-setup-default)
    (setq avy-keys '(?q ?s ?d ?f ?j ?k ?l ?m) ;; AZERTY :)
	  avy-background t)))

;; Google Translate
(use-package google-translate
  :ensure t
  :bind ("C-! w" . google-translate-query-translate)
  :init
  (setq google-translate-default-source-language "en"
	google-translate-default-target-language "fr"
	google-translate-enable-ido-completion t))

;; NSFW
(use-package sudoku
  :ensure t
  :commands sudoku
  :init
  (setq-default sudoku-level "medium"))

;; Deft
(use-package deft
  :ensure t
  :bind ("C-! d" . deft)
  :init
  (progn
    (setq deft-directory "~/Dropbox/deft"
	  deft-extension "org"
	  deft-text-mode 'org-mode
	  deft-use-filename-as-title t)
    (defadvice deft-auto-save (around keep-whitespace-on-deft-auto-save activate)
      (flet ((delete-trailing-whitespace))
	ad-do-it))))

;; ix.io integration
(use-package ix
  :ensure t
  :commands (ix ix-browse ix-delete))

;; Find unbound keys
(use-package unbound
  :ensure t
  :commands describe-unbound-keys)

;; http://www.emacswiki.org/emacs/CamelCase
(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
				     (downcase (match-string 0 s)))
			     t nil s)))
    (downcase s)))

;; Increment number at point
(use-package evil-numbers
  :ensure t
  :bind (("C-! +"             . evil-numbers/inc-at-pt)
	 ("C-! <kp-add>"      . evil-numbers/inc-at-pt)
	 ("C-! -"             . evil-numbers/dec-at-pt)
	 ("C-! <kp-subtract>" . evil-numbers/dec-at-pt)))

;; Unicode fonts
(use-package unicode-fonts
  :ensure t
  :defer 5
  :config
  (progn
    (setq unicode-fonts-block-font-mapping
	  '(("Emoticons"
	     ("Apple Color Emoji" "Symbola" "Quivira"))))
    (unicode-fonts-setup)))

;; Colorize strings that represent colors
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :diminish rainbow-mode)

;;; init-20-utils.el ends here
