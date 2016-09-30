;;; 95-modeline --- modeline configuration
;;; Commentary:
;;; Code:

;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/theme 'respectful
	  sml/show-client t
	  rm-blacklist '(" ,"           ; subword
			 " Abbrev"      ; abbrev
			;" AC"          ; auto-complete
			 " ElDoc"       ; eldoc
			 )
	  sml/replacer-regexp-list
	  '(("^/ssh:.*:" ":SSH:")
	    ("^/sudo:.*:" ":SU:")
	    ("^~/.config/" ":C:")
	    ("^~/Dropbox/" ":DB:")
	    ("^~/stibidik/" ":S:")
	    ("^:S:web/src/stibidik/" ":Sw:")
	    ))

    ;; Move which-func indicator to just after the file name
    ;; https://github.com/Bruce-Connor/smart-mode-line/issues/77#issuecomment-39708760
    (let ((which-func '(which-func-mode ("" which-func-format " ")))
	  cell)
      (setq-default mode-line-format (remove which-func mode-line-format))
      (setq-default mode-line-misc-info (remove which-func mode-line-misc-info))
      (setq cell (last mode-line-format 8))
      (setcdr cell
	      (cons which-func
		    (cdr cell))))

    (sml/setup)
    (display-battery-mode 1)))

;;; init-95-modeline.el ends here
