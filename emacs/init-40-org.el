;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :bind (:map schnouki-prefix-map
	 ("o l" . org-store-link)
	 ("o a" . org-agenda))
  :init
  ;; Various parameters
  (setq org-directory "~/Dropbox/org/"
	org-support-shift-select t
	diary-file (concat org-directory "diary")
	calendar-date-style 'european)

  :config
  ;; Babel
  (add-to-list 'org-babel-load-languages '(ditaa . t))
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (ledger . t)
     (python . t)))
     ;;(sh . t)))
  )

(use-package org-journal
  :ensure t
  :bind (:map schnouki-prefix-map
	 ("o j" . org-journal-new-entry))
  :init
  (setq org-journal-dir (concat org-directory "journal/")
	org-journal-file-type 'weekly
	org-journal-file-format "%Y-%m-%d.org"
	org-journal-date-prefix "* "
	org-journal-date-format "Semaine %W"
	org-journal-time-prefix "** "
	org-journal-time-format ""))

;;; init-40-org.el ends here
