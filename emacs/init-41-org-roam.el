;;; 41-org-roam --- org-roam settings
;;; Commentary:
;;; Code:

(use-package org-roam
  :ensure t
  :diminish org-roam-mode
  :hook
  (after-init . org-roam-mode)

  :bind (:map schnouki-prefix-map
              ("; l" . org-roam)
              ("; f" . org-roam-find-file)
              ("; b" . org-roam-switch-to-buffer)
              ("; g" . org-roam-graph)
	      ("; i" . org-roam-insert)
	      ("; ;" . schnouki/org-roam-dailies-this-week)
	      ("; w n" . schnouki/org-roam-next-week)
	      ("; w p" . schnouki/org-roam-prev-week)
	      ("; w RET" . schnouki/org-roam-week))

  :custom
  (org-roam-directory (concat org-directory "roam/"))
  (org-roam-tag-sources '(prop all-directories))
  (org-roam-capture-templates
   '(("d" "default" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n#+roam_tags:\n\n"
      :unnarrowed t
      :immediate-finish t)))
  (org-roam-dailies-capture-templates
   '(("w" "weekly" plain (function org-roam-capture--get-point)
      ""
      :file-name "%<%Y-w%W>"
      :head "#+TITLE: %<%Y> semaine %<%W> (%<%Y-%m-%d>)\n#+OPTIONS: toc:nil\n\n* Meetings\n* Posts\n* Snippets\n* Capture"
      :immediate-finish t)))

  :config
  (defun schnouki/get-beginning-of-week (&optional time)
    "Get the time of the beginning of the week at TIME."
    (let* ((cur-time (or time (current-time)))
	   (dow (decoded-time-weekday (decode-time cur-time)))
	   (days-since-bow (mod (- dow calendar-week-start-day) 7)))
      (time-subtract cur-time (* days-since-bow 86400))))

  (defun schnouki/org-roam-dailies--file-for-week (time)
    "Create and file weekly org-roam file for TIME."
    (org-roam-dailies--file-for-time (schnouki/get-beginning-of-week time)))

  (defun schnouki/org-roam-dailies-this-week ()
    "Create and find file for this week."
    (interactive)
    (schnouki/org-roam-dailies--file-for-week (current-time)))

  (defun schnouki/org-roam-dailies-next-week (n)
    "Create and find file for next week.
With numeric argument N, use N weeks in the future."
    (interactive "p")
    (schnouki/org-roam-dailies--file-for-week
     (time-add (* n 7 86400) (current-time))))

  (defun schnouki/org-roam-dailies-prev-week (n)
    "Create and find file for the previous week.
With numeric argument N, use N weeks in the past."
    (interactive "p")
    (schnouki/org-roam-dailies-next-week (- n)))

  (defun schnouki/org-roam-dailies-week ()
    "Create the file for any week using the calendar."
    (interactive)
    (let ((time (org-read-date nil 'to-time nil "Date:  ")))
      (schnouki/org-roam-dailies--file-for-week time))))


;; org-capture with org-roam
(defun schnouki/org-roam--this-week-capture ()
  (schnouki/org-roam-this-week)
  (org-capture-put-target-region-and-position)
  (widen)
  (goto-char (point-min))
  (if (re-search-forward (format org-complex-heading-regexp-format "Capture") nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n* Capture\n")
    (beginning-of-line 0)))

(setq org-capture-templates
      '(("s" "Weekly snippets" item (function schnouki/org-roam--this-week-capture)
	 "- %? %c")
	("S" "Weekly snippets link" item (function schnouki/org-roam--this-week-capture)
	 "- %:initial%? [[%:link][%:description]]")))


;; Deft
(use-package deft
  :ensure t
  :after org-roam
  :bind (:map schnouki-prefix-map
	      ("d" . deft))
  :custom
  (deft-auto-save-interval 0)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  (deft-recursive t)
  (deft-strip-summary-regexp
    (rx (group (or
		(: line-start
		   (or
		    (: "#+" (1+ (any upper ?_)) ":" (* nonl)) ;; org-mode metadata
		    (: "- " (1+ (any print)) " ::" (* nonl)))  ;; my org-roam metadata
		   line-end)
		(regexp "[\n\t]") ;; blank
		))))
  (deft-use-filter-string-for-filename t))


;;; init-41-org-roam.el ends here
