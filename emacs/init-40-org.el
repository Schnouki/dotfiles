;;; 40-org --- org-mode
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :bind (:map schnouki-prefix-map
	 ("o l" . org-store-link)
	 ("o a" . org-agenda))
  :init
  ;; Various parameters
  (setq org-directory "~/Dropbox/org/"
	org-support-shift-select t
	diary-file (concat org-directory "diary")
	calendar-date-style 'european)

  :config
  ;; Babel
  (add-to-list 'org-babel-load-languages '(ditaa . t))
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (ledger . t)
     (python . t)))
     ;;(sh . t)))

  (require 'ox-md)
  )

(use-package org-journal
  :ensure t
  :bind (:map schnouki-prefix-map
	 ("o j" . org-journal-new-entry))
  :init
  (setq org-journal-dir (concat org-directory "journal/")
	org-journal-file-type 'weekly
	org-journal-file-format "%Y-%m-%d.org"
	org-journal-date-prefix "* "
	org-journal-date-format "Semaine %W"
	org-journal-time-prefix "** "
	org-journal-time-format ""))

(use-package ox-gfm
  :ensure t
  :after org)


;; Links to GitHub issues and PRs using [[ghi:123]], [[ghpr:user/repo#123]]...
(defvar schnouki/org-gh-default-user nil)
(defvar schnouki/org-gh-default-repo nil)

(defun schnouki/org-gh-parse-url (path)
  (let* ((m (s-match (rx bos (opt
			      (opt (group (1+ alnum)) "/")
			      (group (1+ alnum)) "#")
			 (group (1+ num)) eos)
		     path))
	 (user (or (nth 1 m) schnouki/org-gh-default-user))
	 (repo (or (nth 2 m) schnouki/org-gh-default-repo))
	 (obj (nth 3 m)))
    (list user repo obj)))
(defun schnouki/org-gh-build-url (type path)
  (seq-let (user repo obj) (schnouki/org-gh-parse-url path)
    (concat "https://github.com/" user "/" repo "/" type "/" obj)))
(defun schnouki/org-gh-format-url (path)
  (seq-let (user repo obj) (schnouki/org-gh-parse-url path)
    (cond
     ((not (s-equals? user schnouki/org-gh-default-user))
      (format "%s/%s#%s" user repo obj))
     ((not (s-equals? repo schnouki/org-gh-default-repo))
      (format "%s#%s" repo obj))
     (t (concat "#" obj)))))
(defun schnouki/org-gh-follow (type path)
  (browse-url (schnouki/org-gh-build-url type path)))
(defun schnouki/org-gh-export (type path description format)
  (let ((url (schnouki/org-gh-build-url type path))
	(desc (or description path)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" url desc))
      (`latex (format "\\href{%s}{%s}" url desc))
      (`texinfo (format "@uref{%s,%s}" url desc))
      (`ascii (format "%s (%s)" desc url))
      (`md (format "[%s](%s)" desc url))
      (_ url))))

(defun schnouki/org-ghi-follow (path)
  (schnouki/org-gh-follow "issue" path))
(defun schnouki/org-ghi-export (path description format)
  (schnouki/org-gh-export "issue" path description format))

(defun schnouki/org-ghpr-follow (path)
  (schnouki/org-gh-follow "pull" path))
(defun schnouki/org-ghpr-export (path description format)
  (schnouki/org-gh-export "pull" path description format))

(with-eval-after-load 'org
  (org-link-set-parameters "ghi"
			   :follow #'schnouki/org-ghi-follow
			   :export #'schnouki/org-ghi-export)
  (org-link-set-parameters "ghpr"
			   :follow #'schnouki/org-ghpr-follow
			   :export #'schnouki/org-ghpr-export))


;;; init-40-org.el ends here
