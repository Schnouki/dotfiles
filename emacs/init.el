;;; init.el -- Emacs configuration

;;; Commentary:
;; This file is loaded from ~/.emacs. It must load all init-XX-blabla.el files
;; in ~/.config/emacs (where XX is a number and blabla a short description).

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun schnouki/init-emacs ()
  "Load various Emacs init files."
  (dolist (file (directory-files "~/.config/emacs" t "^init-[0-9]+-.+\.el$"))
    (load file)))

(schnouki/init-emacs)

;;; init.el ends here
