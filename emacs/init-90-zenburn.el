;;; 90-zenburn --- Zenburn color theme
;;; Commentary:
;;; For nice gradients: https://vis4.net/blog/posts/avoid-equidistant-hsv-colors/
;;; Code:

(use-package zenburn-theme
  :ensure t
  :custom
  (zenburn-override-colors-alist '(("zenburn-bg+4" . "#7F7F7F")
                                   ("zenburn-bg+5" . "#8F8F8F")))
  :config
  (load-theme 'zenburn t)

  (zenburn-with-color-variables

    ;; notmuch
    (defun schnouki/get-icon (name)
      (with-temp-buffer
        (insert-file-contents (concat "/usr/share/icons/Numix/16/" name ".svg"))
        (buffer-string)))
    (setq notmuch-search-line-faces `(("deleted" . (:strike-through t))
                                      ("draft"   . (:background ,zenburn-bg-1))
                                      ("sent"    . (:slant italic))
                                      ("unread"  . (:background ,zenburn-bg+1)))
          notmuch-tag-formats `(("attachment" (notmuch-tag-format-image-data tag (schnouki/get-icon "mimetypes/archive")))
                                ("encrypted"  (notmuch-tag-format-image-data tag (schnouki/get-icon "status/keys")))
                                ("flagged"
                                 (propertize tag 'face '(:foreground ,zenburn-blue))
                                 (notmuch-tag-format-image-data tag (schnouki/get-icon "actions/edit-flag")))
                                ("forwarded"  (propertize tag 'face '(:weight bold :foreground ,zenburn-blue-4)))
                                ("inbox"      (propertize tag 'face '(:weight bold)))
                                ("replied"    (propertize tag 'face '(:weight bold :foreground ,zenburn-blue-2)))
                                ("sent"       (propertize tag 'face '(:weight bold :foreground ,zenburn-cyan)))
                                ("todo"       (propertize tag 'face '(:weight bold :foreground ,zenburn-orange)))
                                ("trash"
                                 (propertize tag 'face '(:weight bold :foreground ,zenburn-blue-5))
                                 (notmuch-tag-format-image-data tag (schnouki/get-icon "actions/edit-delete")))
                                ("unread"     (propertize tag 'face '(:foreground ,zenburn-red)))))

    (custom-theme-set-faces
     'user

;;;; Global faces
     `(completions-annotations ((t :foreground ,zenburn-fg-05)))
     `(hl-line ((t :background ,zenburn-bg-05)))
     `(schnouki/notmuch-hl-line ((t :background ,zenburn-bg-05)))
;;;; company
     `(company-preview ((t :background ,zenburn-green+2 :foreground ,zenburn-bg-1)))
;;;; diff, magit-diff
     `(diff-context ((t :foreground "#bcbcac"))) ;; zenburn-fg-2
;;;; forge
     `(forge-dimmed           ((t :foreground ,zenburn-fg-1)))
     `(forge-topic-slug-saved ((t :foreground ,zenburn-orange)))
     `(forge-pullreq-open     ((t :foreground ,zenburn-green)))
     `(forge-pullreq-merged   ((t :foreground "#c8abdd")))
     `(forge-pullreq-rejected ((t :foreground "#c8abdd")))
;;;; go-guru
     `(go-guru-hl-identifier-face ((t :background ,zenburn-bg-1)))
;;;; parinfer
     `(parinfer-rust-dim-parens ((t :foreground ,zenburn-fg-05)))
;;;; scopeline
     `(scopeline-face ((t :foreground ,zenburn-bg+5)))
;;;; show-paren-mode
     `(show-paren-match    ((t :weight bold :foreground ,zenburn-fg :background ,zenburn-bg-1)))
     `(show-paren-mismatch ((t :weight bold :foreground ,zenburn-red :background "#3c1313")))
;;;; smerge-mode
     `(smerge-refined-changed ((t :background ,zenburn-bg+2)))
;;;; sudoku
     `(sudoku-value-face          ((t :foreground ,zenburn-blue)))
     `(sudoku-value-pencil-1-face ((t :foreground ,zenburn-fg-1)))
     `(sudoku-value-pencil-2-face ((t :foreground ,zenburn-fg+1)))
     `(sudoku-autovalue-face      ((t :foreground ,zenburn-magenta)))
;;;; vertico
     `(vertico-current ((t :underline nil)))
;;;; volatile-highlights
     `(vhl/default-face ((t :background "#3D5457")))
     ))
  )


;; rainbow-mode with zenburn colors!
(defun schnouki/rainbow-mode-zenburn ()
  (make-local-variable 'rainbow-x-colors-font-lock-keywords)
  (add-to-list 'rainbow-x-colors-font-lock-keywords
               `(,(regexp-opt (mapcar 'car zenburn-default-colors-alist) 'words)
                 (0 (rainbow-colorize-by-assoc zenburn-default-colors-alist)))
               t)
  (rainbow-mode t))

;; (schnouki/rainbow-mode-zenburn)
;; (rainbow-mode -1)

;; Local Variables:
;; eval: (when (require 'rainbow-mode nil t) (schnouki/rainbow-mode-zenburn))
;; End:
;;; init-90-zenburn.el ends here
