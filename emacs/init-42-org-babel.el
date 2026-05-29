;;; 42-org-babel --- library of Babel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load-feature 'org
  (setopt org-babel-load-languages
          '((emacs-lisp . t)
            (lisp . t)
            (perl . t)
            (plantuml . t)
            (python . t)
            (shell . t)
            (sql . t)
            (sqlite . t))))

(defun schnouki/org-babel-add-language (language)
  (let ((elt (cons language t)))
    (unless (memq elt org-babel-load-languages)
      (setopt org-babel-load-languages
              (cons elt org-babel-load-languages)))))

(use-package ob-mermaid
  :ensure t
  :after org
  :config
  (schnouki/org-babel-add-language 'mermaid))

;;; init-42-org-babel.el ends here
