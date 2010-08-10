;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-! l") 'org-store-link)
(global-set-key (kbd "C-! a") 'org-agenda)
(global-set-key (kbd "C-! b") 'org-iswitchb)
(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key org-mode-map (kbd "C-M-g") 'org-plot/gnuplot)))
(setq-default org-tags-column -80)

;; TODO-list
(defun schnouki/org-todo-list ()
  (interactive)
  (find-file "~/org/todo.org"))
(global-set-key (kbd "C-! t") 'schnouki/org-todo-list)

;; Update from Google Calendar using ical2org
(defun schnouki/org-update-gcal ()
  (interactive)
  (shell-command "~/org/gcal/update.sh"))
(global-set-key (kbd "C-! u") 'schnouki/org-update-gcal)

;; MobileOrg -- http://orgmode.org/manual/MobileOrg.html
(setq org-mobile-directory "/scp:kimsufi:mobileorg/")

(setq org-directory "~/org/"
      org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED"))
      schnouki/org-todo-keywords-sort-order '("DONE" "STARTED" "TODO" "CANCELED")
      org-log-done 'time
      org-enforce-todo-dependencies t
      org-enforce-toto-checkbox-dependencies t
      org-support-shift-select t
      org-agenda-dim-blocked-tasks t
      org-agenda-custom-commands
      '(("c" . "TODO par catégories")
	("cp" "Catégorie: perso" tags-todo "+perso")
	("cj" "Catégorie: pro"   tags-todo "+pro")
	("cd" "Catégorie: dev"   tags-todo "+dev")
	("p" . "TODO par priorités")
	("pa" "Priorité haute"   tags-todo "+PRIORITY=\"A\"")
	("pb" "Priorité normale" tags-todo "+PRIORITY=\"B\"")
	("pc" "Priorité basse"   tags-todo "+PRIORITY=\"C\"")))

(org-remember-insinuate)
(global-set-key (kbd "C-! r") 'org-remember)
(setq org-default-notes-file (concat org-directory "/notes.org")
      org-remember-templates
      '(("TODO" ?t "* TODO %?\n  %i\n  %a"   "todo.org")
        ("Idée" ?i "* %^{Title}\n  %i\n  %a" org-default-notes-file "Idées")))

;; Also use custom faces for checkbox statistics
(eval-after-load 'org-list
  '(progn
     (defun org-get-checkbox-statistics-face ()
       "Select the face for checkbox statistics.
Use the same as for the todo keywods."
       (org-get-todo-face (match-string 1)))))

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
      (org-sort-entries-or-items nil ?a)
      (org-sort-entries-or-items nil ?p)
      (org-sort-entries-or-items nil ?f 'schnouki/org-sort-by-todo-keywords))
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


;; Workaround a bug that causes org-mobile-push to fail when
;; org-todo-keyword-faces is set. Not pretty, but works...
(defadvice org-mobile-push
  (around org-mobile-push-disable-todo-keyword-faces activate)
  (let ((org-todo-keyword-faces nil))
    ad-do-it))
