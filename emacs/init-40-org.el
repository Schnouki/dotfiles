;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin gnu
  :bind (:map org-mode-map
              ("C-c C-x i" . org-id-get-create)
              ("C-c y" . schnouki/org-yank-link))
  :hook (org-mode . visual-line-mode)
  :custom
  (calendar-date-style 'european)
  (calendar-week-start-day 1)
  (diary-file "~/notes/Daily")
  (org-agenda-include-diary t)
  (org-directory "~/notes")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
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
      (org-insert-link nil location desc))))

(use-package company-org-block
  :ensure t
  :hook ((org-mode . (lambda ()
                       (add-to-list (make-local-variable 'company-backends)
                                    'company-org-block)
                       (company-mode +1)))))

(use-package org-roam
  :ensure t
  :after org
  :bind (:map schnouki-prefix-map
              ("r RET" . org-roam-buffer-toggle)
              ("r i" . org-roam-node-insert)
              ("r r a" . org-roam-ref-add)
              ("r r d" . org-roam-ref-remove)
              ("r r f" . org-roam-ref-find))
  :custom
  (org-roam-directory "~/notes")
  :config
  (org-roam-db-autosync-mode 1))

;; Consult & Embark integrations
(use-package consult-org-roam
  :ensure t
  :after consult
  :demand t
  :bind (:map schnouki-prefix-map
              ("r B" . consult-org-roam-backlinks-recursive)
              ("r b" . consult-org-roam-backlinks)
              ("r f" . consult-org-roam-file-find)
              ("r l" . consult-org-roam-forward-links)
              ("r s" . consult-org-roam-search))
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-org-roam-mode 1))

(use-package embark-org-roam
  :ensure t
  :after (embark org-roam)
  :demand t)
;;; init-40-org.el ends here
