;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------

(require 'org-install)
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
      org-log-done 'time
      org-support-shift-select t

      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-hierarchical-todo-statistics nil
      org-hierarchical-checkbox-statistics nil

      org-agenda-dim-blocked-tasks t
      org-agenda-include-diary t
      org-agenda-files (mapcar (lambda (s) (concat org-directory s))
			       '("todo.org" "cours.org"))
      org-agenda-custom-commands
      '(("c" . "TODO par catégories")
	("cp" "Catégorie: perso" tags-todo "+perso")
	("cj" "Catégorie: pro"   tags-todo "+pro")
	("cd" "Catégorie: dev"   tags-todo "+dev")
	("p" . "TODO par priorités")
	("pa" "Priorité haute"   tags-todo "+PRIORITY=\"A\"")
	("pb" "Priorité normale" tags-todo "+PRIORITY=\"B\"")
	("pc" "Priorité basse"   tags-todo "+PRIORITY=\"C\"")))

;; Keyboard shortcuts
(global-set-key (kbd "C-! l") 'org-store-link)
(global-set-key (kbd "C-ç l") 'org-store-link)
(global-set-key (kbd "C-! a") 'org-agenda)
(global-set-key (kbd "C-ç a") 'org-agenda)
(global-set-key (kbd "C-! b") 'org-iswitchb)
(global-set-key (kbd "C-ç b") 'org-iswitchb)
(global-set-key (kbd "C-! t") '(lambda () (interactive) (find-file (concat org-directory "todo.org"))))
(global-set-key (kbd "C-ç t") '(lambda () (interactive) (find-file (concat org-directory "todo.org"))))

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
  "Sort buffer in alphabetical order, then by priority, then by status"
  (interactive)
  (let ((done-regexp (concat "\\<\\("
			     (mapconcat 'regexp-quote org-done-keywords "\\|")
			     "\\)\\>")))
    (save-excursion
      (mark-whole-buffer)
      (org-sort-entries nil ?a)
      (org-sort-entries nil ?p)
      (org-sort-entries nil ?f 'schnouki/org-sort-by-todo-keywords))
    (org-overview)
    (org-content)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward done-regexp nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (hide-subtree))))
))
(global-set-key (kbd "C-! s") 'schnouki/org-sort-todo-list)
(global-set-key (kbd "C-ç s") 'schnouki/org-sort-todo-list)

;; Dynamically adjust tag position 
;; http://orgmode.org/worg/org-hacks.php#sec-13
(setq ba/org-adjust-tags-column t)
(setq schnouki/org-show-ellipsis-when-adjusting-tags t)

(defun ba/org-adjust-tags-column-reset-tags ()
  "In org-mode buffers it will reset tag position according to
`org-tags-column'."
  (when (and
         (not (string= (buffer-name) "*Remember*"))
         (eql major-mode 'org-mode))
    (let ((b-m-p (buffer-modified-p)))
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (command-execute 'outline-next-visible-heading)
            ;; disable (message) that org-set-tags generates
            (flet ((message (&rest ignored) nil))
              (org-set-tags 1 t))
            (set-buffer-modified-p b-m-p))
        (error nil)))))

(defun ba/org-adjust-tags-column-now ()
  "Right-adjust `org-tags-column' value, then reset tag position."
  (set (make-local-variable 'org-tags-column)
       (let ((len (if org-ellipsis (length org-ellipsis) 3)))
	 (if schnouki/org-show-ellipsis-when-adjusting-tags
	     (- (- (window-width) len))
	   (- (window-width)))))
  (ba/org-adjust-tags-column-reset-tags))

(defun ba/org-adjust-tags-column-maybe ()
  "If `ba/org-adjust-tags-column' is set to non-nil, adjust tags."
  (when ba/org-adjust-tags-column
    (ba/org-adjust-tags-column-now)))

(defun ba/org-adjust-tags-column-before-save ()
  "Tags need to be left-adjusted when saving."
  (when ba/org-adjust-tags-column
    (setq org-tags-column 1)
    (ba/org-adjust-tags-column-reset-tags)))

(defun ba/org-adjust-tags-column-after-save ()
  "Revert left-adjusted tag position done by before-save hook."
  (ba/org-adjust-tags-column-maybe)
  (set-buffer-modified-p nil))

; automatically align tags on right-hand side
(add-hook 'window-configuration-change-hook
          'ba/org-adjust-tags-column-maybe)
(add-hook 'before-save-hook 'ba/org-adjust-tags-column-before-save)
(add-hook 'after-save-hook 'ba/org-adjust-tags-column-after-save)
(add-hook 'org-agenda-mode-hook '(lambda ()
				   (setq org-agenda-tags-column (- (window-width)))))
