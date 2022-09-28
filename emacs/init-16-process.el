;;; 16-process --- Best process handling!
;;; Commentary:
;;; Code:

(defvar schnouki/limited-processes-files nil
  "Files for which calls to `make-process' should pass through a wrapper script.

This is a list of symbols representing the base name of the
affected files.")

(defun schnouki/filter-make-process-args (args)
  "Filter ARGS before passing them to `make-process'.

If `make-process' is called from one of the files in
`schnouki/limited-processes-files', the command will be ran
through a wrapper script that adjuts its max allowed memory and
adjusts its OOM score."
  (catch 'limited
    (mapbacktrace
     (lambda (&rest frame)
       (let ((file (symbol-file (cadr frame))))
	 (when (and file
		    (memq (intern (file-name-base file))
			  schnouki/limited-processes-files))
	   (let ((command (map-elt args :command)))
	     (throw 'limited
		    (map-insert args :command
				(append (list
					 (expand-file-name "~/.config/emacs/limited-process-wrapper.sh")
					 (file-name-base file))
					command))))))))
    args))

(advice-add 'make-process :filter-args #'schnouki/filter-make-process-args)
;;(advice-remove 'make-process #'schnouki/filter-make-process-args)

(require 'use-package)

(add-to-list 'use-package-keywords :limit-process)

(defun use-package-normalize/:limit-process (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((listp arg) arg)
       ((and (booleanp arg)
	     arg)
	(list name-symbol))
       ((stringp arg) (list (intern arg)))
       ((symbolp arg)  arg)
       (t
	(use-package-error "Bad argument for :limit-process"))))))

(defun use-package-handler/:limit-process (name-symbol keyword file-names rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (null file-names)
	body
      (use-package-concat
       body
       (mapcar (lambda (file-name)
		 `(add-to-list 'schnouki/limited-processes-files ',file-name))
	       file-names)))))

;;; init-16-process.el ends here
