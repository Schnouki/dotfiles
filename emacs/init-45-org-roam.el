;;; 45-org-roam --- org-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  (auto-save-visited-mode 1)

  ;; Schedule a git sync after saving a note.
  ;; Using add-hook instead of :hook to avoid loading org-roam on every file save.
  (defun schnouki/sync-notes-schedule ()
    "Schedule a notes sync when saving a file in the notes repo."
    (when (and buffer-file-name
               (string-prefix-p (expand-file-name org-roam-directory) buffer-file-name))
      (start-process "sync-notes" nil (expand-file-name "sync-notes" org-roam-directory) "schedule")))
  (add-hook 'after-save-hook #'schnouki/sync-notes-schedule))

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
              ("r j i" . journal-notes-insert-daily-link)
              ("r j n" . journal-notes-goto-next)
              ("r j p" . journal-notes-goto-previous))
  :custom
  (journal-notes-daily-directory "Daily")
  (journal-notes-weekly-directory "Doist/Snippets")
  (journal-notes-daily-template "~/notes/Modèles/Daily.org")
  (journal-notes-weekly-template "~/notes/Modèles/Doist snippets.org"))

;; Highlight unlinked references
(use-package org-roam-latte
  :ensure t
  :after org-roam
  :hook ((org-mode . org-roam-latte-mode))
  :delight)

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

;;; init-45-org-roam.el ends here
