;;; 50-hydra --- hydra config
;;; Commentary:
;;; Code:

(use-package hydra
  :ensure t
  :config
  (require 'hydra-examples))

;; Move to other window
(require 'windmove)
(defhydra hydra-move (global-map "C-$")
  "Move window"
  ("<up>"    windmove-up "up")
  ("<down>"  windmove-down "down")
  ("<left>"  windmove-left "left")
  ("<right>" windmove-right "right"))

;; Move window splitter
(defhydra hydra-move-splitter (global-map "C-£")
  "Move window splitter"
  ("<up>"    hydra-move-splitter-up "up")
  ("<down>"  hydra-move-splitter-down "down")
  ("<left>"  hydra-move-splitter-left "left")
  ("<right>" hydra-move-splitter-right "right"))

;;; init-50-hydra.el ends here
