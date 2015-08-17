;;; 70-services --- Services managed with Prodigy
;;; Commentary:
;;; Code:

(use-package prodigy
  :ensure t
  :bind ("C-! p" . prodigy)
  :config
  (let ((blogdir (expand-file-name "~/site/blog")))
    (prodigy-define-service
      :name "Pelican builder"
      :tags '(blog)
      :cwd blogdir
      :command "pelican"
      :args (list "--debug" "--autoreload" (concat blogdir "/content")
		  "-o" (concat blogdir "/output") "-s" (concat blogdir "/pelicanconf.py"))
      :ready-message "Done: Processed [0-9]+ articles")

    (prodigy-define-service
      :name "Pelican devserver"
      :tags '(blog)
      :cwd (concat blogdir "/output")
      :command "python3"
      :args '("-m" "pelican.server" "9090")
      :port 9090)))

;;; init-70-services.el ends here
