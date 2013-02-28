;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
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
  "Display agenda and todo list in the current window"
  (interactive)
  (delete-other-windows)

  ;; First display todo-list
  (find-file (concat org-directory "todo.org"))

  ;; Then display agenda
  (org-agenda-list)
  (fit-window-to-buffer nil nil 20))

;; Keyboard shortcuts
(global-set-key (kbd "C-! l") 'org-store-link)
(global-set-key (kbd "C-ç l") 'org-store-link)
(global-set-key (kbd "C-! a") 'org-agenda)
(global-set-key (kbd "C-ç a") 'org-agenda)
(global-set-key (kbd "C-! b") 'org-iswitchb)
(global-set-key (kbd "C-ç b") 'org-iswitchb)
(global-set-key (kbd "C-! t") 'schnouki/org-agenda-and-todo-list)
(global-set-key (kbd "C-ç t") 'schnouki/org-agenda-and-todo-list)

;; Capture
(global-set-key (kbd "C-! r") 'org-capture)
(global-set-key (kbd "C-ç r") 'org-capture)
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
(global-set-key (kbd "C-! s") 'schnouki/org-sort-todo-list)
(global-set-key (kbd "C-ç s") 'schnouki/org-sort-todo-list)
