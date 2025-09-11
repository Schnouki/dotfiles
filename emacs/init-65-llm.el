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
  (setq schnouki/llm-claude-provider (schnouki/make-llm-openrouter "anthropic/claude-sonnet-4")
        schnouki/llm-gpt-provider (schnouki/make-llm-openrouter "openai/gpt-4.1")
        schnouki/llm-gemini-provider (schnouki/make-llm-openrouter "google/gemini-2.5-pro-preview")))


;; Magit commit with the help of LLMs
(use-package magit-gptcommit
  :ensure t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider schnouki/llm-claude-provider)
  (magit-gptcommit-prompt "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

Follow good Git style:

- Separate the subject from the body with a blank line
- Try to limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- Use the imperative mood in the subject line
- Wrap the body at 72 characters
- Keep the body short and concise (omit it entirely if not useful)

Diffs:
```
%s
```") ;; From Zed (https://github.com/zed-industries/zed/blob/main/crates/git_ui/src/commit_message_prompt.txt)
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
                        :models '(anthropic/claude-sonnet-4
                                  openai/gpt-4.1
                                  google/gemini-2.5-pro-preview))
        gptel-model 'anthropic/claude-sonnet-4)
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

;; Aider integration
(use-package aider
  :ensure t
  :bind (:map schnouki-prefix-map
              ("l a" . aider-transient-menu))
  :custom
  (aider-args '("--no-auto-accept-architect" "--no-check-update")))

;; GitHub Copilot
(use-package copilot
  :ensure t
  :delight
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion)
              ("C-<right>" . copilot-accept-completion-by-word)
              ("C-<end>" . copilot-accept-completion-by-line)
              ("C-<down>" . copilot-previous-completion)
              ("C-<up>" . copilot-next-completion)))

;;; init-65-llm.el ends here
