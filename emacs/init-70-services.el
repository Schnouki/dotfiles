;;; 70-services --- Services managed with Prodigy
;;; Commentary:
;;; Code:

(use-package prodigy
  :ensure t
  :bind (:map schnouki-prefix-map
	 ("p" . prodigy))
  :config
  (let ((blogdir (expand-file-name "~/blog")))
    (prodigy-define-service
      :name "Hugo server"
      :tags '(blog)
      :cwd blogdir
      :command "make"
      :args '("serve")
      :port 1313)

    (prodigy-define-service
      :name "Hugo server with isso"
      :tags '(blog)
      :cwd blogdir
      :command "make"
      :args '("-j" "serve-isso")
      :port 1313)))

;;; init-70-services.el ends here
