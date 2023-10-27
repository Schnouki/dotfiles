;;; 95-modeline --- modeline configuration
;;; Commentary:
;;; Code:

;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :custom
  (sml/theme 'respectful)
  (sml/show-client t)
  (rm-blacklist '(" ,"           ; subword
                  " Abbrev"      ; abbrev
                  " ElDoc"))      ; eldoc
  (sml/replacer-regexp-list
   '(("^/ssh:.*:"    ":SSH:")
     ("^/sudo:.*:"   ":SU:")
     ("^~/.config/"  ":C:")
     ("^~/Dropbox/"  ":DB:")
     ("^~/doist/"    ":D:")
     ("^:D:Todoist/" ":TD:")
     ("^:D:twist/"   ":TW:")))
  (sml/name-width '(0 . 44))
  (sml/mode-width 'full)

  :config
  ;; Move which-func indicator to just after the file name
  ;; https://github.com/Bruce-Connor/smart-mode-line/issues/77#issuecomment-39708760
  (let ((which-func-entry '(which-function-mode
                            (which-func-mode
                             ("" which-func-format " "))))
        cell)
    (setq mode-line-format (remove which-func-entry mode-line-format))
    (setq mode-line-misc-info (remove which-func-entry mode-line-misc-info))
    (setq cell (last mode-line-format 8))
    (setcdr cell
            (cons which-func-entry
                  (cdr cell))))

  (sml/setup))

;;; init-95-modeline.el ends here
