;;; 41-org-github --- GitHub links in org-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Links to GitHub issues and PRs using ghi:user/repo#123, ghpr:user/repo#123
(defun schnouki/org-gh-parse-url (path)
  (cdr (s-match (rx bos
                    (group (1+ graph))
                    "/"
                    (group (1+ graph))
                    "#"
                    (group (1+ num))
                    eos)
                path)))
(defun schnouki/org-gh-build-url (type path)
  (seq-let (user repo obj) (schnouki/org-gh-parse-url path)
    (concat "https://github.com/" user "/" repo "/" type "/" obj)))
(defun schnouki/org-gh-format-url (path)
  (seq-let (user repo obj) (schnouki/org-gh-parse-url path)
    (format "%s/%s#%s" user repo obj)))
(defun schnouki/org-gh-follow (type path)
  (browse-url (schnouki/org-gh-build-url type path)))
(defun schnouki/org-gh-export (type path description backend)
  (let ((url (schnouki/org-gh-build-url type path))
        (desc (or description
                  (schnouki/org-gh-format-url path))))
    (pcase backend
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" url desc))
      (`latex (format "\\href{%s}{%s}" url desc))
      (`texinfo (format "@uref{%s,%s}" url desc))
      (`ascii (format "%s (%s)" desc url))
      (`md (format "[%s](%s)" (s-replace "#" "\\#" desc) url))
      (`pandoc (org-link-make-string url desc))
      (_ url))))

(defun schnouki/org-ghi-follow (path)
  (schnouki/org-gh-follow "issues" path))
(defun schnouki/org-ghi-export (path description backend)
  (schnouki/org-gh-export "issues" path description backend))

(defun schnouki/org-ghpr-follow (path)
  (schnouki/org-gh-follow "pull" path))
(defun schnouki/org-ghpr-export (path description backend)
  (schnouki/org-gh-export "pull" path description backend))

(with-eval-after-load 'org
  (org-link-set-parameters "ghi"
                           :follow #'schnouki/org-ghi-follow
                           :export #'schnouki/org-ghi-export)
  (org-link-set-parameters "ghpr"
                           :follow #'schnouki/org-ghpr-follow
                           :export #'schnouki/org-ghpr-export))

;;; init-41-org-github.el ends here
