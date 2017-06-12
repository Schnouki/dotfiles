;;; 60-ledger --- stuff for ledger-mode
;;; Commentary:
;;; Code:

(setq ledger-use-iso-dates t)

(defvar schnouki/ledger-account-unknown-prefix "Dépenses:")
(defvar schnouki/ledger-account-unknown-name "Inconnu")

(defun schnouki/ledger-navigate-next-pending-xact ()
  "Move to the beginning of the next pending transaction."
  (interactive)
  (let (found)
    (while (not found)
      (ledger-navigate-next-xact-or-directive)
      (setq found (eq (ledger-transaction-state) 'pending)))))

(defun schnouki/ledger-navigate-previous-pending-xact ()
  "Move to the beginning of the previous pending transaction."
  (interactive)
  (let (found)
    (while (not found)
      (ledger-navigate-previous-xact-or-directive)
      (setq found (eq (ledger-transaction-state) 'pending)))))

(defun schnouki/ledger-goto-unknown-account ()
  "Move to the beginning of the 'unknown account' line in the current transaction."
  (interactive)
  (let ((pos (ledger-navigate-find-xact-extents (point)))
	(acct-name (concat schnouki/ledger-account-unknown-prefix
			   schnouki/ledger-account-unknown-name))
	found ctx)
    (goto-char (car pos))
    (while (not found)
      (forward-line)
      (when (> (point) (cadr pos))
	(error "No 'unknown' account in this transaction!"))
      (setq ctx (ledger-context-at-point)
	    found (and (eq (ledger-context-line-type ctx) 'acct-transaction)
		       (ledger-context-field-value ctx 'account) acct-name)))
    (goto-char (+ (ledger-context-field-position ctx 'account)
		  (length schnouki/ledger-account-unknown-prefix)))
    (set-mark-command nil)
    (goto-char (ledger-context-field-end-position ctx 'account))))

(defun schnouki/ledger-complete-unknown-xact ()
  "Complete a pending transaction with an 'unknown account' line."
  (interactive)
  (schnouki/ledger-goto-unknown-account)
  (delete-active-region)
  (ledger-pcomplete))

(defun schnouki/ledger-clear-xact ()
  "Clear a pending transaction."
  (interactive)
  (ledger-post-align-xact (point))
  (ledger-toggle-current-transaction 'cleared))

(eval-after-load 'ledger-mode
  '(progn
     (defhydra hyda-ledger (ledger-mode-map "C-c l")
       "Common tasks for ledger-mode"
       ("n" ledger-navigate-next-xact-or-directive "next transaction")
       ("p" ledger-navigate-prev-xact-or-directive "previous transaction")
       ("N" schnouki/ledger-navigate-next-pending-xact "next pending transaction")
       ("P" schnouki/ledger-navigate-previous-pending-xact "previous pending transaction")
       ("u" schnouki/ledger-goto-unknown-account "goto 'unknown' account")
       ("x" schnouki/ledger-complete-unknown-xact "complete a pending transaction with an 'unknown' account")
       ("c" schnouki/ledger-clear-xact "clear a pending transaction")
       )
     ))

;;; init-60-ledger.el ends here
