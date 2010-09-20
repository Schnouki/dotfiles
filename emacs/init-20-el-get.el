;; -----------------------------------------------------------------------------
;; Emacs package manager :)
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "~/.config/emacs/el-get/el-get")
(require 'el-get)

(setq el-get-dir "~/.config/emacs/el-get"
      el-get-recipe-path '("~/.config/emacs/el-get/el-get/recipes")
      el-get-sources
      '(el-get google-maps google-weather verbiste
	(:name magit
	       :url "git://github.com/Schnouki/magit.git")
	(:name folding
	       :type http
	       :url "http://git.savannah.gnu.org/cgit/emacs-tiny-tools.git/plain/lisp/other/folding.el?h=devel"
	       :features (folding folding-isearch)
	       :after 'folding-mode-add-find-file-hook)
	(:name pymacs
	       :build ("make" "sudo make install"))
	(:name python-mode
	       :type bzr
	       :url "lp:python-mode")
	))

(el-get)
