;;; 40-tramp --- TRAMP settings
;;; Commentary:
;;; Code:

;; Documentation: https://elpa.gnu.org/packages/doc/tramp.html

;; Make TRAMP a bit faster
;; https://www.gnu.org/software/tramp/#Frequently-Asked-Questions-1
(setopt vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)
        shell-history-file-name t
        tramp-verbose 2)

;; Making TRAMP go brrrr...
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setopt remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1 1024 1024) ;; 1 MB
        magit-tramp-pipe-stty-settings 'pty)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;;; init-40-tramp.el ends here
