;;; 95-modeline --- modeline configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :demand t
  :custom
  (mode-line-compact 'long)
  (sml/theme 'respectful)
  (sml/show-client t)
  (rm-blacklist '(" Abbrev"))
  (sml/replacer-regexp-list
   '(("^/ssh:.*:"    ":SSH:")
     ("^/sudo:.*:"   ":SU:")
     ("^~/.config/"  ":C:")
     ("^~/Dropbox/"  ":DB:")
     ("^~/doist/"    ":D:")
     ("^:D:Todoist/" ":TD:")
     ("^:D:twist/"   ":TW:")))
  (sml/name-width '(0 . 30))
  (sml/mode-width 'full)
  (sml/extra-filler -1)

  :config
  (sml/setup)

  ;; Move which-func indicator to just after the file name
  (when-let* ((which-func-entry (assoc 'which-function-mode mode-line-misc-info)))
    (setq mode-line-format (remove which-func-entry mode-line-format)
          mode-line-misc-info (remove which-func-entry mode-line-misc-info))
    (let ((pos (cl-position 'sml/pos-id-separator mode-line-format)))
      (push which-func-entry (cdr (nthcdr pos mode-line-format))))
    (setq-default mode-line-format mode-line-format)))

;; Manually disable the lighter for some modes
(delight 'subword-mode nil 'subword)

;;; init-95-modeline.el ends here
