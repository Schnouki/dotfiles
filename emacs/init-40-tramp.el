;;; 40-tramp --- TRAMP settings
;;; Commentary:
;;; Code:

;; Share connections with SSH
;; https://elpa.gnu.org/packages/doc/tramp.html#Ssh-setup
(with-eval-after-load 'tramp-sh
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPersist=yes "
                "-o ControlPath=/tmp/ssh-schnouki-%%r@%%h:%%p")))


;;; init-40-tramp.el ends here
