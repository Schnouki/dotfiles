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
    (define-key pelican-keymap (kbd "C-c P d") 'pelican-update-date)

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

;;; init-40-blog.el ends here
