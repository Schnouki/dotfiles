;;; 70-services --- Services managed with Prodigy
;;; Commentary:
;;; Code:

(use-package prodigy
  :ensure prodigy
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
      :args '("-c" "import logging; logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(message)s'); import pelican.server" "9090")
      :ready-message "serving at port [0-9]+")))

;;; init-70-services.el ends here
