;; -----------------------------------------------------------------------------
;; Emacs package manager :)
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "~/.config/emacs/el-get/el-get")
(require 'el-get)

(setq el-get-dir "~/.config/emacs/el-get"
      el-get-recipe-path '("~/.config/emacs/el-get/el-get/recipes")
      el-get-sources
      '(erc-highlight-nicknames flyguess folding google-maps google-weather
	ioccur python-mode rainbow-mode undo-tree verbiste
	(:name el-get
	       :url "git://github.com/Schnouki/el-get.git")
	(:name magit
	       :url "git://github.com/Schnouki/magit.git")
	(:name pymacs
	       :build ("make" "sudo make install"))
	))

(el-get)

(defun schnouki/el-get-update-all ()
  (interactive)
  (let (item)
    (dolist (item el-get-sources)
      (el-get-update (symbol-name
		      (if (symbolp item) item
			(plist-get item :name)))))))

