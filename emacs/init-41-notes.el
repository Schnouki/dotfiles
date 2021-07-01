;;; 41-notes --- note-taking with deft and zetteldeft
;;; Commentary:
;;; Code:

;; Keymap for Deft and org-roam
(defvar schnouki/notes-prefix-map (make-sparse-keymap))
(bind-key ";" schnouki/notes-prefix-map schnouki-prefix-map)

;; Deft
(use-package deft
  :ensure t
  :bind (:map schnouki/notes-prefix-map
	      ("RET" . deft)
	      ("R" . deft-refresh)
	      )
  :custom
  (deft-auto-save-interval 0)
  (deft-extensions '("org" "md" "txt"))
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/Notes")
  (deft-recursive t)
  (deft-strip-summary-regexp
    (rx (1+ (or
		(: line-start
		   (or
		    (: "#+" (1+ (any upper ?_)) ":" (* nonl)) ;; org-mode metadata
		    (: "#" (1+ space) "Tags" (* nonl)) ;; zetteldeft tags
		    (: "- " (1+ (any print)) " ::" (* nonl)))  ;; my org-roam metadata
		   line-end)
		(regexp "[\n\t]") ;; blank
		))))
  (deft-use-filename-as-title t))

;; org-roam and friends
(use-package org-roam
  :ensure t
  :after org
  :hook (org-load . org-roam-mode)
  :bind (:map schnouki/notes-prefix-map
	      ("f" . org-roam-find-file)
	      ("F" . org-roam-find-directory)
	      ("i" . org-roam-insert)
	      ("c" . org-roam-capture)
	      ("b" . org-roam-switch-to-buffer)
	      ("t" . org-roam-buffer-toggle-display)
	      ("r" . org-roam)
	      ("d d" . org-roam-dailies-find-today)
	      ("d RET" . org-roam-dailies-find-date)
	      ("D D" . org-roam-dailies-capture-today)
	      ("D RET" . org-roam-dailies-capture-date)
	      )
  :custom
  (org-roam-directory org-directory)
  (org-roam-verbose nil)
  (org-roam-completion-systen 'ivy)
  (org-roam-tag-sources '(prop all-directories))
  (org-roam-buffer-window-parameters '((no-other-window . t)
				       (no-delete-other-windows . t)))

  (org-roam-capture-templates
   `(("d" "default" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+TITLE: ${title}\n#+roam_alias:\n#+roam_tags:\n\n"
      :unnarrowed t))
   )
  (org-roam-capture-immediate-template
   (append (car org-roam-capture-templates) '(:immediate-finish t)))

  (org-roam-dailies-capture-templates
   `(("d" "default" entry (function org-roam-capture--get-point)
      "* %?"
      :file-name ,(concat org-roam-dailies-directory "%<%Y-%m-%d>")
      :head "#+TITLE: %<%Y-%m-%d>\n")
     ("s" "snippets" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "doist/snippets/%(format-time-string \"%yw%W\" (schnouki/get-beginning-of-week))"
      :head ,(concat "#+TITLE: Snippets %(format-time-string \"%yw%W\" (schnouki/get-beginning-of-week))\n"
		    "#+OPTIONS: toc:nil ^:nil H:0\n\n"
		    "* Meetings\n* Posts\n* Weekly commitment\nI commit to <...> by the end of the week.\n* Snippets\n")
      :unnarrowed t
      :immediate-finish t
      :jump-to-captured t)))
  )

(use-package org-noter
  :ensure t
  :commands (org-noter))

;; Weekly snippets
(defun schnouki/get-beginning-of-week (&optional time)
  "Get the time of the beginning of the week at TIME."
  (let* ((cur-time (or time (current-time)))
	 (dow (decoded-time-weekday (decode-time cur-time)))
	 (days-since-bow (mod (- dow calendar-week-start-day) 7)))
    (time-subtract cur-time (* days-since-bow 86400))))

(defun schnouki/find-snippet-file ()
  "Open the snippet file for this week."
  (interactive)
  (require 'org-roam-dailies)
  (let ((org-roam-dailies-capture-templates
	 (--filter (string= (car it) "s") org-roam-dailies-capture-templates)))
    (org-roam-dailies-find-today)))

(bind-key ";" 'schnouki/find-snippet-file schnouki/notes-prefix-map)

;;; init-41-notes.el ends here
