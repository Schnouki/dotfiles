;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :bind (:map schnouki-prefix-map
	 ("o l" . org-store-link)
	 ("o a" . org-agenda)
	 :map org-mode-map
	 ("C-c C-x i" . org-id-get-create)
	 ("C-c y" . schnouki/org-yank-link))
  :custom
  (org-adapt-indentation nil)
  (org-checkbox-hierarchical-statistics nil)
  (org-cycle-separator-lines 3)
  (org-directory "~/Dropbox/Notes/")
  (diary-file "~/Dropbox/Notes/diary")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (calendar-date-style 'european)
  (calendar-week-start-day 1)

  :init
  (defun schnouki/org-mode-hook ()
    (electric-indent-local-mode -1))
  ;;(add-hook 'org-mode-hook #'schnouki/org-mode-hook)
  ;;(remove-hook 'org-mode-hook #'schnouki/org-mode-hook)

  :config
  ;; Babel
  (add-to-list 'org-babel-load-languages '(ditaa . t))
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (python . t)))
  ;;(sh . t)))

  (defun schnouki/org-yank-link (&optional description)
    "Yank a link."
    (interactive "P")
    (let* ((location (current-kill 0))
	   (desc (cond
		  (current-prefix-arg (completing-read (format "Description for %s: " location) nil))
		  (description description)
		  (t "ref"))))
      (org-insert-link nil location desc)))
  )

(use-package ox-md
  :after org)
(use-package ox-gfm
  :ensure t
  :after org)

(use-package org-contrib
  :ensure t
  :after org)

(use-package company-org-block
  :ensure t
  :after (org company)
  :custom
  (company-org-block-edit-style 'auto)
  :hook (org-mode . (lambda ()
		       (add-to-list (make-local-variable 'company-backends)
				    'company-org-block))))

;;; init-40-org.el ends here
