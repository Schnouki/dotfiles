;;; 65-llm --- Interaction with Large Language Models
;;; Commentary:
;;; Code:

;; Generic LLM capabilities
(use-package llm
  :ensure t
  :custom
  (llm-warn-on-nonfree nil)
  :config
  (require 'llm-openai)

  (cl-defun schnouki/make-llm-openrouter (chat-model &key default-chat-max-tokens)
    (let ((api-key (auth-info-password (car (auth-source-search :host "openrouter.ai"
                                                                :user "apikey")))))
      (make-llm-openai-compatible :chat-model chat-model
                                  :default-chat-max-tokens default-chat-max-tokens
                                  :url "https://openrouter.ai/api/v1/"
                                  :key api-key)))
  (setq schnouki/llm-claude-provider (schnouki/make-llm-openrouter "anthropic/claude-3.7-sonnet:beta")
        schnouki/llm-gpt-provider (schnouki/make-llm-openrouter "openai/gpt-4o")
        schnouki/llm-llama-provider (schnouki/make-llm-openrouter "meta-llama/llama-3.1-70b-instruct")))


;; Magit commit with the help of LLMs
(use-package magit-gptcommit
  :ensure t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider schnouki/llm-claude-provider)
  (magit-gptcommit-prompt "You are generating a commit message for code changes. Analyze these diffs:

```
%s
```

Respond ONLY with the commit message following these rules:

SUBJECT LINE:
- Use imperative mood (\"Add\", not \"Added\")
- Max 50 characters
- Capitalize first word
- No period at end
- Complete this: \"If applied, this commit will...\"

DESCRIPTION (only if needed):
- Separate from subject with blank line
- Wrap at 72 characters
- Explain WHY, not HOW
- Use Markdown if helpful
- Skip if subject is self-explanatory

Output format:
[subject line]

[optional description]")
  (magit-gptcommit-max-token 100000)
  :config
  ;; Add gptcommit transient commands to `magit-commit'
  (magit-gptcommit-status-buffer-setup))


;; GPTel with Kagi FastGPT and OpenRouter.ai models
(use-package gptel
  :ensure t
  :commands (gptel-request gptel-make-kagi)
  :bind (:map schnouki-prefix-map
              ("l RET" . gptel-menu)
              ("l n" . gptel)
              ("l s" . gptel-send))
  :config
  (require 'gptel-curl)
  (gptel-make-kagi "Kagi" :key #'gptel-api-key)
  (setq gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key #'gptel-api-key
                        :models '(anthropic/claude-3.7-sonnet:beta
                                  openai/gpt-4o
                                  meta-llama/llama-3.1-70b-instruct
                                  qwen/qwen-2.5-72b-instruct))
        gptel-model 'anthropic/claude-3.7-sonnet:beta)
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
                 :system (format "You are large language model and a careful %s programmer. \
Provide code and only code as an output, without any additional text, prompt \
or note. Use docstrings, but keep them short. Use comment for non-obvious \
parts of the code. Do not wrap output in a markdown code block." lang)))

(bind-key "l c" #'schnouki/gptel-write-code schnouki-prefix-map)

;; Aidermacs
(use-package aidermacs
  :ensure t
  :bind (:map schnouki-prefix-map
              ("l a" . aidermacs-transient-menu))

  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-default-model "openrouter/anthropic/claude-3.7-sonnet:beta")
  (aidermacs-extra-args '("--no-check-update")))

;;; init-65-llm.el ends here
