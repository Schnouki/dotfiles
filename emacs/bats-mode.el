(require 'derived)

(define-derived-mode bats-mode shell-script-mode
  "bats mode"
  "Major mode for editing Bats (Bash Automated Testing System) files"
  (sh-set-shell "/bin/bash")
  (font-lock-add-keywords
   nil
   '(("\\(^@test\\|\\<\\(run\\|load\\|skip\\)\\)\\>" . font-lock-keyword-face))))

(provide 'bats-mode)
