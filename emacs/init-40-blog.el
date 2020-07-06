;;; 40-blog --- blogging configuration
;;; Commentary:
;;; Code:

(setq schnouki/blog-basedir "~/blog/")

(defun schnouki/markdown-maybe-add-shortcode-keyword ()
  "Enable fontifying Hugo shortcodes if in the ~/blog/ directory."
  (when (string= (projectile-project-root) (expand-file-name schnouki/blog-basedir))
    (let ((shortcode-regexp
           (rx (group "{{" (or "<" "%") (1+ space))            ; opening {{< or {{%
               (group (1+ (not space)))                        ; shortcode name
               (group (*? any))                                ; parameters
               (group (1+ space) (? "/") (or ">" "%") "}}")))) ; closing >}}, %}}, />}} or /%}}
      (font-lock-add-keywords nil `((,shortcode-regexp . ((1 'markdown-markup-face)
                                                          (2 'markdown-metadata-key-face)
                                                          (3 'markdown-metadata-value-face)
                                                          (4 'markdown-markup-face))))))))

(add-hook 'markdown-mode-hook #'schnouki/markdown-maybe-add-shortcode-keyword)


;; Useless :)
;; (use-package company-emoji
;;   :ensure t
;;   :commands company-emoji-init
;;   :hook (markdown-mode . company-emoji-init)
;;   :custom
;;   (company-emoji-insert-unicode nil))

;;; init-40-blog.el ends here
