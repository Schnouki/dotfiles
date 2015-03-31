;;; 60-multiple-cursors --- multiple cursors
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :ensure t
  :bind (("C-* l" . mc/edit-lines)
	 ("C-* n" . mc/mark-next-like-this)
	 ("C-* p" . mc/mark-previous-like-this)
	 ("C-* C-*" . mc/mark-all-like-this)
	 ("C-c C-* C-*" . mc/mark-more-like-this)

	 ("C-* i" . mc/insert-numbers)
	 ("C-* s" . mc/sort-regions)
	 ("C-* r" . mc/reverse-regions)
	 ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :config
  (require 'mc-extras))

(use-package mc-extras
  :ensure t
  :commands (mc/compare-chars mc/compare-chars-backward mc/compare-chars-forward
			      mc/cua-rectangle-to-multiple-cursors
			      mc/remove-current-cursor mc/remove-duplicated-cursors)
  :config
  (progn
    (bind-keys :map mc/keymap
	       ("C-. C-d" . mc/remove-current-cursor)
	       ("C-. d" . mc/remove-duplicated-cursors)
	       ("C-. =" . mc/compare-chars))
    (eval-after-load 'cua-base
      '(bind-key "C-. C-," 'mc/cua-rectangle-to-multiple-cursors cua--rectangle-keymap))))

;;; init-60-multiple-cursors.el ends here
