;; -----------------------------------------------------------------------------
;; Multiple cursors
;; -----------------------------------------------------------------------------

(require 'multiple-cursors)

(global-set-key (kbd "C-* l") 'mc/edit-lines)
(global-set-key (kbd "C-* n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-* p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-* C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-* C-*") 'mc/mark-more-like-this)

(global-set-key (kbd "C-* i") 'mc/insert-numbers)
(global-set-key (kbd "C-* s") 'mc/sort-regions)
(global-set-key (kbd "C-* r") 'mc/reverse-regions)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
