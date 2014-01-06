;;; 70-services --- Services managed with Prodigy
;;; Commentary:
;;; Code:

(require 'prodigy)
(global-set-key (kbd "C-! p") 'prodigy)

;; Personal stuff
(let ((blogdir (expand-file-name "~/site/blog")))
  (prodigy-define-service
    :name "Pelican builder"
    :cwd blogdir
    :command "pelican"
    :args `("--debug" "--autoreload" ,(concat blogdir "/content")
	    "-o" ,(concat blogdir "/output") "-s" ,(concat blogdir "/pelicanconf.py"))
    :tags '(blog))

  (prodigy-define-service
    :name "Pelican devserver"
    :cwd (concat blogdir "/output")
    :command "python3"
    :args '("-m" "pelican.server" "9090")
    :tags '(blog)))

;;; init-70-services.el ends here
