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
  (org-checkbox-hierarchical-statistics nil)
  (org-directory "~/Dropbox/org/")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-support-shift-select t)
  (org-cycle-separator-lines 3)
  (org-startup-with-inline-images t)
  (calendar-date-style 'european)
  (calendar-week-start-day 1)
  (diary-file (concat org-directory "diary"))

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
     (ledger . t)
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

(use-package org-brain
  :ensure t
  :after org
  :bind (:map schnouki-prefix-map
	      ("; b" . org-brain-visualize)
	      ("; r" . org-brain-refile))
  :custom
  (org-brain-backlink "← "))


;;; init-40-org.el ends here
