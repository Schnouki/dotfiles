;;; 65-llm --- Interaction with Large Language Models
;;; Commentary:
;;; Code:

;; GitHub Copilot
(use-package copilot
  :ensure t
  :delight
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion)
              ("C-<right>" . copilot-accept-completion-by-word)
              ("C-<end>" . copilot-accept-completion-by-line)
              ("C-<down>" . copilot-previous-completion)
              ("C-<up>" . copilot-next-completion)))

;; Agent-shell
(use-package agent-shell
  :ensure t)

;;; init-65-llm.el ends here
