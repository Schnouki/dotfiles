;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-! l" . org-store-link)
	 ("C-ç l" . org-store-link)
	 ("C-! a" . org-agenda)
	 ("C-ç a" . org-agenda)
	 ("C-! b" . org-iswitchb)
	 ("C-ç b" . org-iswitchb)
	 ("C-! t" . schnouki/org-agenda-and-todo-list)
	 ("C-ç t" . schnouki/org-agenda-and-todo-list)
	 ("C-! r" . org-capture)
	 ("C-ç r" . org-capture)
	 ("C-! s" . schnouki/org-sort-todo-list)
	 ("C-ç s" . schnouki/org-sort-todo-list)))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key org-mode-map (kbd "C-M-g") 'org-plot/gnuplot)))

;; Various parameters
(setq-default org-tags-column -80)
(setq org-directory "~/Dropbox/org/"
      org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED"))
      schnouki/org-todo-keywords-sort-order '("DONE" "STARTED" "TODO" "CANCELED")
      org-highest-priority ?A
      org-default-priority ?C
      org-lowest-priority  ?E
      org-log-done 'time
      org-support-shift-select t

      org-agenda-window-setup 'other-window

      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-hierarchical-todo-statistics nil
      org-hierarchical-checkbox-statistics nil

      org-agenda-include-diary nil
      diary-file (concat org-directory "diary")
      calendar-date-style 'european

      org-agenda-dim-blocked-tasks t
      org-agenda-files (mapcar (lambda (s) (concat org-directory s ".org"))
			       '("todo"))
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
	("pe" "Priorité très basse" tags-todo "+PRIORITY=\"E\"")))

(defun schnouki/org-agenda-and-todo-list ()
  "Display agenda and todo list in the current window."
  (interactive)
  (delete-other-windows)

  ;; First display todo-list
  (find-file (concat org-directory "todo.org"))

  ;; Then display agenda
  (org-agenda-list)
  (fit-window-to-buffer nil nil 20))

;; Capture
(setq org-default-notes-file (concat org-directory "/notes.org")
      org-capture-templates
      '(("t" "TODO" entry (file "todo.org") "* TODO %?\n  %i\n  %a" :unnarrowed)
        ("i" "Idée" entry (file) "* Idée : %^{Titre} (%T)\n  %i\n  %a" :unnarrowed)))

;; Get the index of a todo keyword in the schnouki/org-todo-keywords-sort-order list
;; Used to sort a todo-list by todo order
(defun schnouki/org-sort-by-todo-keywords ()
  (interactive)
  (if (looking-at org-complex-heading-regexp)
      (string-position (match-string 2) schnouki/org-todo-keywords-sort-order)))

;; Sort todo-list
(defun schnouki/org-sort-todo-list ()
  "Sort buffer in alphabetical order, then by status, then by priority."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (org-sort-entries nil ?a)
    (org-sort-entries nil ?f 'schnouki/org-sort-by-todo-keywords)
    (org-sort-entries nil ?p))
  (org-overview))

;;; init-40-org.el ends here
