;; -----------------------------------------------------------------------------
;; Small useful functions and key bindings
;; -----------------------------------------------------------------------------

;; Copy current line with M-k
;; http://www.emacsblog.org/2009/05/18/copying-lines-not-killing/#comment-27462
(defun schnouki/copy-line (&optional arg)
  (interactive "P")
  (kill-ring-save (line-beginning-position) (+ 1 (line-end-position))))
(global-set-key (kbd "M-k") 'schnouki/copy-line)

;; Switch to scratch buffer, creating it if necessary
;; http://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs/776052#776052
(defun schnouki/goto-scratch (&optional force-new)
  "Switch to scratch buffer, creating it if necessary. Calling
this function with a prefix forces the creation of a new buffer."
  (interactive "P")
  (let ((sb (if force-new
	       (generate-new-buffer "*scratch*")
	     (get-buffer-create "*scratch*"))))
    (switch-to-buffer sb)
    (lisp-interaction-mode)))
(global-set-key (kbd "C-x M-s") 'schnouki/goto-scratch)

;; buffer-menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; "Smart" home key
;; Beginning of indented text --> beginning of "real" text --> beginning of line
(defun schnouki/home-key ()
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

;; Quick diff between current buffer and file
;; From http://slashusr.wordpress.com/2010/01/19/quickly-diff-the-changes-made-in-the-current-buffer-with-its-file/
(defun schnouki/diff-current-buffer-with-file ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(global-set-key (kbd "C-x =") 'schnouki/diff-current-buffer-with-file)

;; Enlarge/shrink window horozontally/vertically
(global-set-key (kbd "C-M-j") 'shrink-window)
(global-set-key (kbd "C-M-k") 'enlarge-window)
(global-set-key (kbd "C-M-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-l") 'enlarge-window-horizontally)

;; Convert seconds to a duration
(defun schnouki/seconds-to-duration (seconds)
  "Convert seconds to a readable duration"
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
(setq schnouki/immortal-star-buffers '("^\\*scratch\\*")
      schnouki/immortal-modes        '(message-mode notmuch-hello-mode notmuch-search-mode
				       notmuch-show-mode org-agenda-mode inferior-python-mode))
(defun schnouki/kill-star-buffers ()
  "Remove most star-buffers (`*Messages*', `*Compilation', ...) that are not in the `schnouki/immortal-star-buffers' list."
  (interactive)
  (let ((count 0)
	buf-name buf-mode)
    (dolist (buf (buffer-list))
      (setq buf-name (buffer-name buf)
	    buf-mode (with-current-buffer buf major-mode))
      (when (and (string-match "^\\*.+$" buf-name)
		 (notany '(lambda (re) (string-match re buf-name)) schnouki/immortal-star-buffers)
		 (not (memq buf-mode schnouki/immortal-modes)))
	(kill-buffer buf)
	(setq count (1+ count))))
    (message (concat (int-to-string count) " buffers killed"))))
(global-set-key (kbd "C-x M-k") 'schnouki/kill-star-buffers)

;; undo-tree
(require 'undo-tree)
;; Lighter displayed in mode line
(setq undo-tree-mode-lighter " UT")
;; ...and enable!
(global-undo-tree-mode)

;; ace-jump (reminder: C-x C-SPC to pop-global-mark)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-'") 'ace-jump-mode)
(setq ace-jump-mode-scope 'frame)

;; http://irreal.org/blog/?p=760
(add-hook 'ace-jump-mode-before-jump-hook
	  (lambda () (push-mark (point) t))) ;until it's fixed in Maramalade

;; ioccur - incremental search of lines in current buffer matching input
(add-to-list 'desktop-globals-to-save 'ioccur-history)
(global-set-key (kbd "C-! C-s") 'ioccur)
(global-set-key (kbd "C-! M-s") 'ioccur-find-buffer-matching)

;; Google Translate
(autoload 'google-translate-query-translate "google-translate" nil t)
(global-set-key (kbd "C-! w") 'google-translate-query-translate)
(setq google-translate-default-source-language "en"
      google-translate-default-target-language "fr"
      google-translate-enable-ido-completion t)

;; NSFW
(autoload 'sudoku "sudoku" nil t)
(setq sudoku-level "medium")

;; Deft
(setq deft-directory "~/Dropbox/deft"
      deft-extension "org"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t)
(global-set-key (kbd "C-! d") 'deft)
(defadvice deft-auto-save (around keep-whitespace-on-deft-auto-save activate)
  (flet ((delete-trailing-whitespace))
    ad-do-it))
