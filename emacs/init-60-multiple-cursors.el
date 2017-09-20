;;; 60-multiple-cursors --- multiple cursors
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :ensure t
  :commands (mc/add-cursor-on-click
	     mc/edit-beginning-of-lines
	     mc/edit-lines
	     mc/insert-numbers
	     mc/mark-all-dwim
	     mc/mark-all-in-region-regexp
	     mc/mark-all-like-this
	     mc/mark-next-like-this
	     mc/mark-previous-like-this
	     mc/mark-sgml-tag-pair
	     mc/reverse-regions
	     mc/skip-to-next-like-this
	     mc/skip-to-previous-like-this
	     mc/sort-regions
	     mc/unmark-next-like-this
	     mc/unmark-previous-like-this)
  )

(defhydra hydra-mc (:hint nil)
  "
      ^Up^            ^Down^        ^All^                ^Lines^               ^Edit^                 ^Other^
----------------------------------------------------------------------------------------------------
[_p_]   Next    [_n_]   Next    [_a_] All like this  [_l_] Edit lines      [_i_] Insert numbers   [_t_] Tag pair
[_P_]   Skip    [_N_]   Skip    [_r_] All by regexp  [_L_] Edit line beg.  [_s_] Sort regions      ^ ^
[_M-p_] Unmark  [_M-n_] Unmark  [_d_] All DWIM        ^ ^                  [_R_] Reverse regions  [_q_] Quit
"
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)

  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)

  ("a" mc/mark-all-like-this :exit t)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("d" mc/mark-all-dwim :exit t)

  ("l" mc/edit-lines :exit t)
  ("L" mc/edit-beginnings-of-lines :exit t)

  ("i" mc/insert-numbers)
  ("s" mc/sort-regions)
  ("R" mc/reverse-regions)

  ("t" mc/mark-sgml-tag-pair)
  ("q" nil)

  ("<mouse-1>" mc/add-cursor-on-click)
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore))

(bind-key "C-*" 'hydra-mc/body)

;;; init-60-multiple-cursors.el ends here
