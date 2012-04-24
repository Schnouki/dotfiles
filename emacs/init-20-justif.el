;; -----------------------------------------------------------------------------
;; Text justification
;; -----------------------------------------------------------------------------

;; Set justification with C-x M-f
(global-set-key (kbd "C-x M-f") 'set-justification)

;; Avoid sentences that end with 2 spaces (American style).
;; TODO: change this automatically according to the current dictionary
(setq sentence-end-double-space nil)

;; Avoid breaking lines at '(' or ':' characters
(add-hook 'fill-no-break-predicate 'fill-french-nobreak-p)

;; Justify at 80 columns
(setq-default fill-column 80)

;; Auto-fill for LaTeX
(defun schnouki/latex-auto-fill ()
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq default-justification 'left))
(add-hook 'LaTeX-mode-hook 'schnouki/latex-auto-fill)
