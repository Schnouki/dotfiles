;;; 80-modeline --- modeline configuration
;;; Commentary:
;;; Code:

;; Remove some lighters
(dolist (mode '(auto-dim-other-buffers-mode hs-minor-mode subword-mode))
  (let ((entry (assq mode minor-mode-alist)))
    (when entry (setcdr entry '(nil)))))

;;; init-80-modeline.el ends here
