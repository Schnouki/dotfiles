;;; 18-bind --- Everything needed to bind keys
;;; Commentary:
;;; Code:

(use-package bind-key
  :ensure t)

(use-package hydra
  :ensure t)

(defvar schnouki-prefix-map (make-sparse-keymap)
  "Prefix map for my personal commands.")
(defvar schnouki-global-map (make-sparse-keymap)
  "Global map for my personal commands.")

(bind-key* "C-!" schnouki-prefix-map)
(bind-key* "C-/" schnouki-prefix-map)

;;; init-18-bind.el ends here
