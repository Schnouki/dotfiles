;;; init.el -- Emacs configuration

;;; Commentary:
;; This file is loaded from ~/.emacs. It must load all init-XX-blabla.el files
;; in ~/.config/emacs (where XX is a number and blabla a short description).

;;; Code:

(defvar schnouki/sd-notify--client nil)
(defvar schnouki/sd-notify--queue '())

(defun schnouki/sd-notify--connect ()
  (unless schnouki/sd-notify--client
    (let ((addr (getenv "NOTIFY_SOCKET")))
      (cond (addr
             (setq schnouki/sd-notify--client
                   (make-network-process :name "sd-notify"
                                         :type 'datagram
                                         :family 'local
                                         :service addr
                                         :sentinel 'schnouki/sd-notify--sentinel
                                         :coding 'utf-8
                                         :noquery t)))
            (t (message "Can't notify systemd: NOTIFY_SOCKET is not set"))))))

(defun schnouki/sd-notify--maybe-dump-queue ()
  (when (and schnouki/sd-notify--client
             (eq (process-status schnouki/sd-notify--client) 'open))
    (dolist (item schnouki/sd-notify--queue)
      (process-send-string schnouki/sd-notify--client item))
    (setq schnouki/sd-notify--queue '())))

(defun schnouki/sd-notify--sentinel (proc ev)
  (schnouki/sd-notify--maybe-dump-queue))

(defun schnouki/sd-notify (state)
  "Notify systemd that the service state changed to STATE."
  (schnouki/sd-notify--connect)
  (add-to-list 'schnouki/sd-notify--queue state t)
  (schnouki/sd-notify--maybe-dump-queue))

(defun schnouki/init-emacs ()
  "Load various Emacs init files."
  (advice-add 'server-start :after #'schnouki/init-emacs--notify-ready)
  (dolist (file (directory-files "~/.config/emacs" t "^init-[0-9]+-.+\.el$"))
    (schnouki/sd-notify (format "STATUS=Loading %s" file))
    (load file)))

(defun schnouki/init-emacs--notify-ready (&rest args)
  (schnouki/sd-notify "STATUS=Ready\nREADY=1"))

(schnouki/init-emacs)
;(package-initialize)

;;; init.el ends here
