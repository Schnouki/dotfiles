;;; 30-yasnippet --- yasnippet
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer 15
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))

(defun schnouki/add-yasnippet-dir (dir)
  (with-eval-after-load 'yasnippet
    (unless (member dir yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs dir)
      (yas-reload-all))))

(use-package yasnippet-snippets
  :ensure t)

(use-package consult-yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-c & C-v" . consult-yasnippet-visit-snippet-file)
              ("C-c & C-s" . consult-yasnippet)
              ("C-c & RET" . consult-yasnippet)))

;;; init-30-yasnippet.el ends here
