;;; 60-multiple-cursors --- multiple cursors
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines
             mc/mark-all-like-this
             mc/mark-next-like-this
             mc/skip-to-next-like-this
             mc/unmark-next-like-this
             mc/mark-previous-like-this
             mc/skip-to-previous-like-this
             mc/unmark-previous-like-this
             mc/vertical-align
             mc/mark-all-in-region-regexp
             mc/insert-numbers
             mc/insert-letters
             mc/add-cursor-on-click)
  :autoload mc/num-cursors)

;; https://github.com/abo-abo/hydra/wiki/multiple-cursors
(defhydra hydra-mc (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
^^-------------- ^^-------------- ^^^^------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR^^     [Click] Cursor at point"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("|" mc/vertical-align)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

(bind-key "C-*" 'hydra-mc/body)

(defun schnouki/whitelist-hydra-mc-functions ()
  (unless mc--list-file-loaded
    (mc/load-lists))
  (dolist (head hydra-mc/heads)
    (let* ((func (cadr head))
           (is-exit (plist-get (cdddr head) :exit))
           (hydra-func (intern (concat "hydra-mc/"
                                       (symbol-name func)
                                       (if is-exit "-and-exit" "")))))
      (when (member func mc--default-cmds-to-run-for-all)
        (add-to-list 'mc/cmds-to-run-for-all hydra-func))
      (when (member func mc--default-cmds-to-run-once)
        (add-to-list 'mc/cmds-to-run-once hydra-func))))
  (mc/save-lists))

(with-eval-after-load 'multiple-cursors
  (schnouki/whitelist-hydra-mc-functions))

;;; init-60-multiple-cursors.el ends here
