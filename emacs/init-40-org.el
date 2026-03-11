;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin gnu
  :bind (:map org-mode-map
              ("C-c C-x i" . org-id-get-create)
              ("C-c y" . schnouki/org-yank-link)
              :map schnouki-prefix-map
              ("o a" . org-agenda)
              ("o c" . org-capture)
              ("o l" . org-store-link))
  :hook (org-mode . visual-line-mode)
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

  (with-eval-after-load 'zenburn-theme
    (zenburn-with-color-variables
      (setq org-todo-keyword-faces `(("WIP" . ,zenburn-orange)
                                     ("READY" . ,zenburn-blue))))))

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
  (auto-save-visited-interval 15)
  (auto-save-visited-predicate #'org-roam-buffer-p)
  (org-roam-directory "~/notes")
  (org-roam-capture-templates '(("g" "generic" plain "%?"
                                 :if-new (file+head "${title}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("d" "doist" plain "%?"
                                 :if-new (file+head "Doist/${title}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("p" "person" plain (file "~/notes/Modèles/Personne.org")
                                 :if-new (file+head "People/${title}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("a" "article" plain (file "~/notes/Modèles/Article.org")
                                 :if-new (file+head "Articles/${title}.org" ":PROPERTIES:\n:ROAM_REFS: ${article}\b:END:\n#+title: ${title}\n")
                                 :unnarrowed t)
                                ("v" "video" plain (file "~/notes/Modèles/Vidéo.org")
                                 :if-new (file+head "Vidéos/${title}.org" ":PROPERTIES:\n:ROAM_REFS: ${video}\b:END:\n#+title: ${title}\n")
                                 :unnarrowed t)
                                ("l" "livre" plain (file "~/notes/Modèles/Livre.org")
                                 :if-new (file+head "Livres/${title}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("t" "tech" plain (file "~/notes/Modèles/Tech.org")
                                 :if-new (file+head "Tech/${title}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("c" "cours" plain (file "~/notes/Modèles/Cours.org")
                                 :if-new (file+head "Cours/${title}.org" ":PROPERTIES:\n:ROAM_REFS: ${video}\b:END:\n#+title: ${title}\n")
                                 :unnarrowed t)
                                ("r" "recette" plain (file "~/notes/Modèles/Recette.org")
                                 :if-new (file+head "${title}.org" ":PROPERTIES:\n:ROAM_REFS: ${source}\b:END:\n#+title: ${title}\n")
                                 :unnarrowed t)
                                ))
  :config
  (org-roam-db-autosync-mode 1)

  ;; Auto-save notes
  (auto-save-visited-mode 1))

(use-package journal-notes
  :load-path "~/.config/emacs"
  :after org
  :bind (:map schnouki-prefix-map
              ("r j j" . journal-notes-goto-today)
              ("r j d" . journal-notes-goto-date)
              ("r j t" . journal-notes-goto-tomorrow)
              ("r j y" . journal-notes-goto-yesterday)
              ("r j w" . journal-notes-goto-this-week)
              ("r j RET" . journal-notes-goto-date)
              ("r j W" . journal-notes-goto-week)
              ("r j i" . journal-notes-insert-daily-link))
  :custom
  (journal-notes-daily-directory "Daily")
  (journal-notes-weekly-directory "Doist/Snippets")
  (journal-notes-daily-template "~/notes/Modèles/Daily.org")
  (journal-notes-weekly-template "~/notes/Modèles/Doist snippets.org"))

;; Consult & Embark integrations
(use-package consult-org-roam
  :ensure t
  :after consult
  :demand t
  :delight
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

;; Export backends
(use-package ox-pandoc
  :ensure t
  :after ox
  :custom
  (org-pandoc-options-for-commonmark '((wrap . "none")))
  (org-pandoc-options-for-gfm '((wrap . "none")))
  (org-pandoc-menu-entry
   '( ;; From https://github.com/emacsorphanage/ox-pandoc/blob/master/ox-pandoc.el
     (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
     (?$ "as html5." org-pandoc-export-as-html5)
     (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
     (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
     (?: "to rst and open." org-pandoc-export-to-rst-and-open)
     (?* "as rst." org-pandoc-export-as-rst)
     (?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
     (?A "as asciidoc." org-pandoc-export-as-asciidoc)
     (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
     (?G "as gfm." org-pandoc-export-as-gfm)
     (?m "to man and open." org-pandoc-export-to-man-and-open)
     (?M "as man." org-pandoc-export-as-man)
     (?c "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
     (?C "as commonmark." org-pandoc-export-as-commonmark)
     (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
     (?R "as rtf." org-pandoc-export-as-rtf)
     (?, "to typst and open." org-pandoc-export-to-typst-and-open)
     (?, "as typst." org-pandoc-export-as-typst)
     (?< "to typst-pdf." org-pandoc-export-to-typst-pdf)
     (?< "to typst-pdf and open." org-pandoc-export-to-typst-pdf-and-open)
     (?x "to docx and open." org-pandoc-export-to-docx-and-open)
     (?X "to docx." org-pandoc-export-to-docx))))

(defun schnouki/org-to-clipboard ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((org-text (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-substring-no-properties (point-min) (point-max))))
           (html (org-export-string-as org-text 'html t '(:with-toc nil))))
      (call-process-region html nil "wl-copy" nil 0 nil
                           "-n" "-t" "text/html"))))
(bind-key "o e c" #'schnouki/org-to-clipboard schnouki-prefix-map)

;; Import, too!
(use-package org-pandoc-import
  :load-path "~/.config/emacs/org-pandoc-import"
  :after org
  :bind (:map schnouki-prefix-map
              ("o i p" . org-pandoc-import-as-org)
              ("o i P" . org-pandoc-import-to-org)))

(defun schnouki/org-import-commonmark-to-org ()
  "Convert CommonMark from the clipboard to Org and insert at point."
  (interactive)
  (when-let ((text (current-kill 0 t)))
    (let ((org-text
           (with-temp-buffer
             (insert text)
             (let ((exit-code
                    (call-process-region (point-min) (point-max)
                                         org-pandoc-import-executable
                                         t t nil
                                         "--wrap=none"
                                         "-f" "commonmark"
                                         "-t" "org")))
               (unless (zerop exit-code)
                 (user-error "Pandoc failed (exit code %d): %s"
                             exit-code (buffer-string)))
               (buffer-string)))))
      (insert org-text))))
(bind-key "o i c" #'schnouki/org-import-commonmark-to-org schnouki-prefix-map)

;;; init-40-org.el ends here
