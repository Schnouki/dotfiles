;;; 35-window --- Deal with Emacs windows
;;; Commentary:
;;; Code:

;; Move to other window
(require 'windmove)
(defhydra hydra-move (global-map "C-$")
  "Move window"
  ("<up>"    windmove-up "up")
  ("<down>"  windmove-down "down")
  ("<left>"  windmove-left "left")
  ("<right>" windmove-right "right"))

;; Move window splitter
(require 'hydra-examples)
(defhydra hydra-move-splitter (global-map "C-&")
  "Move window splitter"
  ("<up>"    hydra-move-splitter-up "up")
  ("<down>"  hydra-move-splitter-down "down")
  ("<left>"  hydra-move-splitter-left "left")
  ("<right>" hydra-move-splitter-right "right"))

;;; init-35-window.el ends here
