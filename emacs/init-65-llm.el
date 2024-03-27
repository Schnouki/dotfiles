;;; 65-llm --- Interaction with Large Language Models
;;; Commentary:
;;; Code:

;; GPTel with Kagi FastGPT
(use-package gptel
  :ensure t
  :commands (gptel-request gptel-make-kagi)
  :bind (:map schnouki-prefix-map
              ("l RET" . gptel)
              ("l s" . gptel-send))
  :config
  (require 'gptel-curl)
  (setq gptel-backend (gptel-make-kagi "Kagi" :key #'gptel-api-key)
        gptel-model "fastgpt")
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-log-level 'nil))

(defun schnouki/gptel-write-code (prompt lang)
  "Write code in LANG according to PROMPT."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "LLM directive: "))
    (if (derived-mode-p 'prog-mode)
        (thread-last (symbol-name major-mode)
                     (string-remove-suffix "-mode")
                     (string-remove-suffix "-ts"))
      (read-string "Programming language: "))))
  (gptel-request prompt
    :system (format "You are large language model and a careful %s programmer. Provide code and only code as an output, without any additional text, prompt or note, but with relevant comments or docstrings. Do not wrap output in a markdown code block." lang)))

(bind-key "l c" #'schnouki/gptel-write-code schnouki-prefix-map)

;;; init-65-llm.el ends here
