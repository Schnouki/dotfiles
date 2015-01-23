;;; 95-modeline --- modeline configuration
;;; Commentary:
;;; Code:

;; Smart mode line
(use-package smart-mode-line
  :ensure smart-mode-line
  :init
  (progn
    (setq sml/theme 'respectful
	  sml/show-client t
	  rm-blacklist '(" ,"           ; subword
			 " Abbrev"      ; abbrev
			;" AC"          ; auto-complete
			 " ElDoc"       ; eldoc
			 " hs"          ; hideshow
			 )
	  sml/replacer-regexp-list
	  '(("^/ssh:.*:" ":SSH:")
	    ("^/sudo:.*:" ":SU:")
	    ("^~/.config/" ":C:")
	    ("^~/Dropbox/" ":DB:")
	    ("^~/findspire/findspire-front/" ":FS:")
	    ("^:FS:findspire/" ":FSf:")
	    ))
    (sml/setup)
    (display-battery-mode 1)))

;;; init-95-modeline.el ends here
