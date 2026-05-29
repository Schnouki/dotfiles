;;; 41-org-import --- org-mode import  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org-pandoc-import
  :load-path "~/.config/emacs/org-pandoc-import"
  :after org
  :bind (:map schnouki-prefix-map
              ("o i p" . org-pandoc-import-as-org)
              ("o i P" . org-pandoc-import-to-org)))

(defun schnouki/org-import-with-pandoc (text format)
  "Convert TEXT in FORMAT to Org and insert at point."
  (require 'org-pandoc-import)
  (let ((org-text
         (with-temp-buffer
           (insert text)
           (let ((exit-code
                  (call-process-region (point-min) (point-max)
                                       org-pandoc-import-executable
                                       t t nil
                                       "--wrap=none"
                                       "-f" format
                                       "-t" "org")))
             (unless (zerop exit-code)
               (user-error "Pandoc failed (exit code %d): %s"
                           exit-code (buffer-string)))
             (buffer-string)))))
    (insert org-text)))

(defun schnouki/org-import-commonmark-to-org ()
  "Convert CommonMark from the clipboard to Org and insert at point."
  (interactive)
  (schnouki/org-import-with-pandoc (current-kill 0 t) "commonmark"))
(bind-key "o i c" #'schnouki/org-import-commonmark-to-org schnouki-prefix-map)

(defun schnouki/org-import-richtext-to-org ()
  "Convert rich text from the clipboard to Org and insert at point."
  (interactive)
  (schnouki/org-import-with-pandoc (gui-get-selection 'CLIPBOARD 'text/html) "html"))
(bind-key "o i r" #'schnouki/org-import-richtext-to-org schnouki-prefix-map)

;;; init-41-org-import.el ends here
