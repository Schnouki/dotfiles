;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-! l" . org-store-link)
	 ("C-! a" . org-agenda)
	 ("C-! b" . org-iswitchb)
	 ("C-! t" . schnouki/org-agenda-and-todo-list))
  :config
  (progn
    (bind-key "C-M-g" 'org-plot/gnuplot org-mode-map)
    (add-to-list 'org-babel-load-languages '(ditaa . t))))

(add-hook 'org-mode-hook 'turn-on-font-lock)

;; Various parameters
(setq-default org-tags-column -80)
(setq org-directory "~/Dropbox/org/"
      org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "DÉLÉGUÉ" "ANNULÉ"))
      schnouki/org-todo-keywords-sort-order '("DONE" "STARTED" "TODO" "DÉLÉGUÉ" "ANNULÉ")
      org-highest-priority ?A
      org-default-priority ?C
      org-lowest-priority  ?E
      org-log-done nil
      org-support-shift-select t

      org-agenda-window-setup 'other-window

      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-hierarchical-todo-statistics nil
      org-checkbox-hierarchical-statistics nil

      org-agenda-include-diary nil
      diary-file (concat org-directory "diary")
      calendar-date-style 'european

      org-agenda-dim-blocked-tasks t
      org-agenda-files (mapcar (lambda (s) (concat org-directory s ".org"))
			       '("todo" "agenda"))
      org-agenda-custom-commands
      '(("c" . "TODO par catégories")
	("cp" "Catégorie: perso" tags-todo "+perso")
	("cj" "Catégorie: pro"   tags-todo "+pro")
	("cd" "Catégorie: dev"   tags-todo "+dev")
	("p" . "TODO par priorités")
	("pa" "Priorité très haute" tags-todo "+PRIORITY=\"A\"")
	("pb" "Priorité haute"      tags-todo "+PRIORITY=\"B\"")
	("pc" "Priorité normale"    tags-todo "+PRIORITY=\"C\"")
	("pd" "Priorité basse"      tags-todo "+PRIORITY=\"D\"")
	("pe" "Priorité très basse" tags-todo "+PRIORITY=\"E\""))

      org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (ledger . t)
   (python . t)
   (sh . t)))

(setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . (lambda (url link) (browse-url url)))
		      ("\\.pdf\\'" . default)))

(defun schnouki/org-agenda-and-todo-list ()
  "Display agenda and todo list in the current window."
  (interactive)
  (delete-other-windows)

  ;; First display todo-list
  (find-file (concat org-directory "todo.org"))

  ;; Then display agenda
  (org-agenda-list)
  (fit-window-to-buffer nil nil 20))

;;; init-40-org.el ends here
