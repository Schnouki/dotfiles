;;; 10-fonts --- Fonts!
;;; Commentary:
;;; Code:

;; Default font
(let ((default-font "Iosevka-8"))
  (assq-delete-all 'font default-frame-alist)
  (add-to-list 'default-frame-alist
	       `(font . ,default-font))
  (set-frame-font default-font)
  (set-face-attribute 'fixed-pitch nil :family default-font))

;;; init-10-fonts.el ends here
