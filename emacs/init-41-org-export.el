;;; 41-org-export --- org-mode export -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ox-pandoc
  :ensure t
  :after ox
  :custom
  (org-pandoc-options-for-commonmark '((wrap . "none")))
  (org-pandoc-options-for-gfm '((wrap . "none")))
  (org-pandoc-menu-entry
   '( ;; From https://github.com/emacsorphanage/ox-pandoc/blob/master/ox-pandoc.el
     (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
     (?$ "as html5." org-pandoc-export-as-html5)
     (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
     (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
     (?: "to rst and open." org-pandoc-export-to-rst-and-open)
     (?* "as rst." org-pandoc-export-as-rst)
     (?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
     (?A "as asciidoc." org-pandoc-export-as-asciidoc)
     (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
     (?G "as gfm." org-pandoc-export-as-gfm)
     (?m "to man and open." org-pandoc-export-to-man-and-open)
     (?M "as man." org-pandoc-export-as-man)
     (?c "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
     (?C "as commonmark." org-pandoc-export-as-commonmark)
     (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
     (?R "as rtf." org-pandoc-export-as-rtf)
     (?, "to typst and open." org-pandoc-export-to-typst-and-open)
     (?, "as typst." org-pandoc-export-as-typst)
     (?< "to typst-pdf." org-pandoc-export-to-typst-pdf)
     (?< "to typst-pdf and open." org-pandoc-export-to-typst-pdf-and-open)
     (?x "to docx and open." org-pandoc-export-to-docx-and-open)
     (?X "to docx." org-pandoc-export-to-docx))))

(defun schnouki/org-to-clipboard ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((org-text (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-substring-no-properties (point-min) (point-max))))
           (html (org-export-string-as org-text 'html t '(:with-toc nil))))
      (call-process-region html nil "wl-copy" nil 0 nil
                           "-n" "-t" "text/html"))))
(bind-key "o e c" #'schnouki/org-to-clipboard schnouki-prefix-map)

;;; init-41-org-export.el ends here
