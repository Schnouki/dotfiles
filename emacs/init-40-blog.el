;;; 40-blog --- blogging configuration
;;; Commentary:
;;; Code:

(use-package pelican-mode
  :load-path "~/.config/emacs/pelican-mode"
  :commands (pelican-mode pelican-enable-if-site)
  :init
  (progn
    (add-hook 'markdown-mode-hook 'pelican-enable-if-site)
    (add-hook 'rst-mode-hook 'pelican-enable-if-site))
  :config
  (progn
    (unbind-key (kbd "C-c P t") pelican-keymap)
    (unbind-key (kbd "C-c P u") pelican-keymap)

    (defun pelican-update-date-with-filename ()
      "Update the date in the current file name and updats its date header."
      (interactive)
      (let ((current-file-name (file-name-nondirectory (buffer-file-name)))
	    (re-date-prefix (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " ")))
	(when (string-match-p re-date-prefix current-file-name)
	  (let* ((new-file-name (concat (format-time-string "%Y-%m-%d ")
					(substring current-file-name 11)))
		 (full-name (concat (file-name-directory (buffer-file-name))
				    new-file-name)))
	    (when (not (string= (buffer-file-name) full-name))
	      (rename-file (buffer-file-name) full-name)
	      (set-visited-file-name full-name nil t)))))
      (pelican-update-date))
    (define-key pelican-keymap (kbd "C-c P d") 'pelican-update-date-with-filename)

    (defun pelican-update-modified (&optional date)
      "Update a Pelican modification date header."
      (interactive "P")
      (unless date
	(setq date (pelican-timestamp-now)))
      (let* ((mdwn (pelican-is-markdown))
	     (mod-header (if mdwn "Modified" ":modified"))
	     (date-header (if mdwn "Date" ":date"))
	     (re-mod (format "^%s: [-0-9 :]+\n" mod-header))
	     (re-date (format "^%s: [-0-9 :]+\n" date-header)))
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward re-mod nil t)
	      (replace-match (format "%s: %s\n" mod-header date))
	    (if (re-search-forward re-date nil t)
		(progn
		  ;(goto-char (match-end 0))
		  (insert mod-header ": " date "\n"))
	      (error "Could not find the modified nor the date header"))))))
    (define-key pelican-keymap (kbd "C-c P m") 'pelican-update-modified)))

;; Useless :)
(use-package company-emoji
  :ensure t
  :commands company-emoji-init
  :config
  (setq company-emoji-insert-unicode nil)
  :init
  (add-hook 'markdown-mode-hook 'company-emoji-init))

;;; init-40-blog.el ends here
