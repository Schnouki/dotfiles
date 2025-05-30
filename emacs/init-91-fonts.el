;;; 91-fonts --- Fonts!
;;; Commentary:
;;; Code:

(defun schnouki/setup-fonts (&rest args)
  "Setup fonts."
  (interactive)
  (let* ((mono-family "Iosevka")
         (mono-serif-family "Iosevka Slab")
         (var-family "Iosevka Aile")
         (font-size 11.5)
         (mono-font (format "%s-%d" mono-family font-size)))

    (custom-set-faces `(default ((t (:family ,mono-family))))
                      `(fixed-pitch ((t (:family ,mono-family))))
                      `(fixed-pitch-serif ((t (:family ,mono-serif-family))))
                      `(variable-pitch ((t (:family ,var-family)))))

    (assq-delete-all 'font default-frame-alist)
    (add-to-list 'default-frame-alist
                 `(font . ,mono-font))
    (set-frame-font mono-font)

    (seq-each (lambda (face) (set-face-attribute face nil :font mono-font))
              '(mode-line mode-line-active mode-line-inactive))))

(with-eval-after-load 'faces
  (schnouki/setup-fonts)

  (defface strictly-fixed-pitch
    '((t :inherit fixed-pitch
         :font "Iosevka Term"))
    "A strictly fixed-pitch face, even for graphical Unicode characters"
    :group 'basic-faces))

(add-hook 'after-init-hook #'schnouki/setup-fonts)
(add-hook 'enable-theme-functions #'schnouki/setup-fonts)


;;; init-91-fonts.el ends here
