;;; 70-services --- Services managed with Prodigy
;;; Commentary:
;;; Code:

(require 'prodigy)
(global-set-key (kbd "C-! p") 'prodigy)

;; Personal stuff
(let ((blogdir (expand-file-name "~/site/blog")))
  (prodigy-define-service
    :name "Pelican builder"
    :tags '(blog)
    :cwd blogdir
    :command "pelican"
    :args `("--debug" "--autoreload" ,(concat blogdir "/content")
	    "-o" ,(concat blogdir "/output") "-s" ,(concat blogdir "/pelicanconf.py"))
    :on-output (lambda (service output)
		 (when (s-matches? "Done: Processed [0-9]+ articles" output)
		   (prodigy-set-status service 'ready))))

  (prodigy-define-service
    :name "Pelican devserver"
    :tags '(blog)
    :cwd (concat blogdir "/output")
    :command "python3"
    :args '("-m" "pelican.server" "9090")))

;;; init-70-services.el ends here
