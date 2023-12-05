;;; 40-vc --- version control
;;; Commentary:
;;; Code:

(defvar schnouki-magit-prefix-map (make-sparse-keymap)
  "Prefix map for Magit and other Git-related commands.")

(bind-key "<" schnouki-magit-prefix-map schnouki-prefix-map)
(bind-key "\\" schnouki-magit-prefix-map schnouki-prefix-map)

(use-package magit
  :ensure t
  :bind (("C-<"      . magit-status)
         ("C-\\"     . magit-status)
         :map schnouki-magit-prefix-map
         ("<" . magit-dispatch)
         ("\\" . magit-dispatch)
         ("f" . magit-file-dispatch)
         ("l" . magit-log-buffer-file)
         ("b" . magit-blame))
  :demand t
  :custom
  (magit-process-popup-time 2)
  (vc-follow-symlinks t)
  (epg-pinentry-mode nil)
  :config

  (setq schnouki/magit-process-buffer-tail t)
  (defun schnouki/toggle-magit-process-buffer-tail ()
    (interactive)
    (setq schnouki/magit-process-buffer-tail (not schnouki/magit-process-buffer-tail)))

  (defun schnouki/magit-process--maybe-tail-buffer (proc string)
    (let ((window (get-buffer-window (process-buffer proc))))
      (when window
        (with-selected-window window
          (end-of-buffer)
          (recenter -1 t)))))

  (advice-add #'magit-process-filter :after #'schnouki/magit-process--maybe-tail-buffer))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-link
  :ensure t
  :bind (:map schnouki-magit-prefix-map
              ("g g" . git-link)
              ("g c" . git-link-commit)
              ("g h" . git-link-homepage))
  :custom
  (git-link-use-commit t))

;; git modes
(use-package git-modes
  :ensure t)

;; Forge
(use-package forge
  :ensure t
  :after magit)

;; Use SSH agent
(setenv "SSH_AUTH_SOCK"
        (format "/run/user/%d/gnupg/S.gpg-agent.ssh" (user-uid)))

;;; init-40-vc.el ends here
