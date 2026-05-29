;;; 40-org --- org-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin gnu
  :bind (:map org-mode-map
              ("C-c C-x i" . org-id-get-create)
              ("C-c y" . schnouki/org-yank-link)
              ("S-RET" . schnouki/org-shift-ret)
              ("S-<return>" . schnouki/org-shift-ret)
              :map schnouki-prefix-map
              ("o a" . org-agenda)
              ("o c" . org-capture)
              ("o l" . org-store-link))
  :hook (org-mode . schnouki/visual-text-mode--enable)
  :custom
  (calendar-date-style 'european)
  (calendar-week-start-day 1)
  (diary-file "~/notes/Daily")
  (org-agenda-include-diary t)
  (org-directory "~/notes")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-return-follows-link t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)

  :config
  (defun schnouki/org-yank-link (&optional description)
    "Yank a link."
    (interactive "P")
    (let* ((location (current-kill 0))
           (desc (cond
                  (current-prefix-arg (completing-read (format "Description for %s: " location) nil))
                  (description description)
                  (t "ref"))))
      (org-insert-link nil location desc)))

  (defun schnouki/org-insert-subitem (&optional checkbox)
    "Insert a new item at the current level and indent it.

`CHECKBOX' is handled as in `org-insert-item'."
    (interactive "P")
    (when (and (bolp) (not (eobp)) (not (eolp))) (forward-char))
    (org-insert-item checkbox)
    (org-indent-item))

  (defun schnouki/org-shift-ret (&optional arg)
    (interactive "P")
    ;; Default org-mode behavior for S-RET: org-table-copy-down
    (call-interactively (cond ((org-at-table-p) #'org-table-copy-down)
                              ((org-in-item-p) #'schnouki/org-insert-subitem)
                              (t #'org-insert-subheading))))

  (with-eval-after-load 'zenburn-theme
    (zenburn-with-color-variables
      (setq org-todo-keyword-faces `(("WIP" . ,zenburn-orange)
                                     ("READY" . ,zenburn-blue)))))

  (delight 'org-capture-mode))

(use-package company-org-block
  :ensure t
  :hook ((org-mode . (lambda ()
                       (add-to-list (make-local-variable 'company-backends)
                                    'company-org-block)
                       (company-mode +1)))))

;;; init-40-org.el ends here
