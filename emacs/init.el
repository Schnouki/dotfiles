;; init.el
;;
;; This file is loaded from ~/.emacs. It must load all init-XX-blabla.el files
;; in ~/.config/emacs (where XX is a number and blabla a short description).

(defun schnouki/init-emacs ()
  "Load various emacs init files"
  (interactive)
  (dolist (file (directory-files "~/.config/emacs" nil "^init-[0-9]+-.+\.el$"))
    (load (concat "~/.config/emacs/" file))))
(schnouki/init-emacs)
