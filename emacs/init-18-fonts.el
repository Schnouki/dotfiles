;;; 18-fonts --- Modern font features: ligatures, Emoji...
;;; Commentary:
;;; Code:

;; From https://masteringemacs.org/article/unicode-ligatures-color-emoji

(use-package ligature
  :load-path "~/.config/emacs/ligature.el"
  :disabled t
  :config
  ;; Enable Iosevka "calt" ligatures
  (ligature-set-ligatures
   'prog-mode
   '("<--" "<---" "<<-" "<-" "<->" "->" "->>" "-->" "--->" "<!--"
     "<-->" "<--->" "<---->"
     "<==" "<===" "<<=" "<=" "<=>" "=>" "=>>" "==>" "===>" "<!---"
     ">=" ">>=" "<==>" "<===>" "<====>"
     "<-------" "------->" "<======>" "<~~" "<~" "~>" "~~>"
     "==" "!=" "===" "!=="
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:"
     "::" ":::" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; init-18-fonts.el ends here
