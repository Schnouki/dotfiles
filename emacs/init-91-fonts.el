;;; 91-fonts --- Fonts!
;;; Commentary:
;;; Code:

(require 'dash)

(defun schnouki/setup-fonts ()
  "Setup fonts."
  (interactive)
  (let* ((mono-family "Iosevka")
	 (mono-serif-family "Iosevka Slab")
	 (var-family "Iosevka Aile")
	 (font-size 12)
	 (mono-font (format "%s-%d" mono-family font-size))
	 (mono-serif-font (format "%s-%d" mono-serif-family font-size))
	 (var-font (format "%s-%d" var-family font-size)))

    (set-face-attribute 'default nil :font mono-font)
    (set-face-attribute 'fixed-pitch nil :font mono-font)
    (set-face-attribute 'fixed-pitch-serif nil :font mono-serif-font)
    (set-face-attribute 'variable-pitch nil :font var-font)

    (assq-delete-all 'font default-frame-alist)
    (add-to-list 'default-frame-alist
		 `(font . ,mono-font))
    (set-frame-font mono-font)

    (--each '(mode-line mode-line-active mode-line-inactive)
      (set-face-attribute it nil :font mono-font))))

(eval-after-load 'faces
  (schnouki/setup-fonts))


;;; init-91-fonts.el ends here
