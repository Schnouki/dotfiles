;;; 25-utils --- Small useful functions and key bindings
;;; Commentary:
;;; Code:

;; which-key helps a lot
(use-package which-key
  :ensure t
  :delight
  :custom
  (which-key-show-docstrings t)
  (which-key-max-description-length nil)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;; A better *help* buffer
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         :map schnouki-prefix-map
         ("h" . helpful-at-point))
  :init
  (setq counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable))

;; Set justification with C-x M-f
(bind-key "C-x M-f" 'set-justification)

;; Tweak visual-line stuff
(setq line-move-visual nil)
(bind-key "C-x t" 'toggle-truncate-lines)

;; Auto-update buffers when the file changes on-disk
(global-auto-revert-mode 1)
(delight 'auto-revert-mode)
(bind-key "C-x r RET" 'revert-buffer)

;; And also auto-revert dired buffers (needed since global-auto-revert-mode only
;; works for buffers associated with files on the disk)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; http://emacsrocks.com/e16.html
(setq dired-dwim-target t)

;; http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("I" . dired-subtree-remove)))

(use-package dired-collapse
  :ensure t
  :commands (dired-collapse dired-collapse-mode)
  :hook dired-mode)

;; Copy current line with M-k
;; http://www.emacsblog.org/2009/05/18/copying-lines-not-killing/#comment-27462
(defun schnouki/copy-line ()
  "Copy the current line to the `kill-ring'."
  (interactive)
  (kill-ring-save (line-beginning-position) (+ 1 (line-end-position))))
(bind-key "M-k" 'schnouki/copy-line)

;; Kill beginning of line
(defun schnouki/kill-beginning-of-line ()
  "Kill the beginning of the current line, up to the point."
  (interactive)
  (save-mark-and-excursion
    (kill-region (line-beginning-position) (point))))
(bind-key "M-K" 'schnouki/kill-beginning-of-line)

;; Copy the region after trimming it
(defun schnouki/kill-ring-save-trimmed (beg end)
  "Save the region after trimming it as if killed, but don't kill it."
  (interactive (list (mark) (point)))
  (let ((str (filter-buffer-substring beg end)))
    (kill-new (s-trim str)))
  (setq deactivate-mark t)
  nil)
(bind-key "M-W" 'schnouki/kill-ring-save-trimmed)

;; Browse kill ring
(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring)

;; Better yank
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" browse-kill-ring "list"))
(bind-key "C-y" 'hydra-yank-pop/yank)
(bind-key "M-y" 'hydra-yank-pop/yank-pop)

;; Viking-mode
(use-package viking-mode
  :ensure t
  :delight
  :config
  (viking-global-mode t))

;; Switch to scratch buffer, creating it if necessary
;; http://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs/776052#776052
(defun schnouki/goto-scratch (&optional force-new mode)
  "Switch to a scratch buffer, creating it if necessary.
Calling this function with a prefix FORCE-NEW forces the creation
of a new buffer.  If MODE is nil, create a buffer in
`initial-major-mode'.  If t, use the current `major-mode'.  If a
symbol, use that mode instead."
  (interactive "P")
  (let* ((buffer-mode (if (booleanp mode)
                          (if mode major-mode
                            initial-major-mode)
                        mode))
         (buffer-name (concat "*scratch"
                              (when (not (eq buffer-mode initial-major-mode))
                                (concat ":" (symbol-name buffer-mode)))
                              "*"))
         (buffer (if force-new
                     (generate-new-buffer buffer-name)
                   (get-buffer-create buffer-name))))
    (switch-to-buffer buffer)
    (when (not (eq major-mode buffer-mode))
      (funcall buffer-mode))))

(defun schnouki/goto-scratch-mode (&optional prefix)
  "Switch to a scratch buffer, letting the user decide its major mode.
If PREFIX is not nil, force creating a new scratch buffer."
  (interactive "P")
  (let* ((modes (->> auto-mode-alist
                     (-map 'cdr)
                     -distinct))
         (default-mode (symbol-name major-mode))
         (prompt (concat "Major mode (" default-mode "): "))
         (chosen-mode (completing-read prompt modes nil nil nil nil default-mode)))
    (schnouki/goto-scratch prefix (intern chosen-mode))))

(bind-key "C-x M-s" 'schnouki/goto-scratch)
(bind-key "C-x M-d" 'schnouki/goto-scratch-mode)

;; ibuffer
(bind-key "C-x C-b" 'ibuffer)

;; "Smart" home key
;; Beginning of indented text --> beginning of "real" text --> beginning of line
(defun schnouki/home-key ()
  "'Smart' home key manager."
  (interactive "^")
  (let
      ((pos-current (current-column))                                ;; Current position
       (pos-indent (progn (back-to-indentation) (current-column)))   ;; Beginning of indented text
       (pos-real (progn (beginning-of-line-text) (current-column)))) ;; Beginning of real text

    ;; If at beginning of the indented text and if it's not the same as real
    ;; text, go to real text
    (if (and (= pos-current pos-indent) (not (= pos-indent pos-real)))
        (move-to-column pos-real)
      ;; Else, if at beginning of real text, go to beginning of line
      (if (= pos-current pos-real) (move-to-column 0)
        ;; Else, go to beginning of indented text
        (move-to-column pos-indent)))))
(global-set-key [home] 'schnouki/home-key)

;; Insert newline and return to point
(defun schnouki/newline-same-point ()
  "Insert newline and return to point."
  (interactive)
  (save-excursion
    (newline-and-indent))
  (funcall indent-line-function))
(bind-key "M-RET" 'schnouki/newline-same-point)

;; Quick diff between current buffer and file
;; From http://slashusr.wordpress.com/2010/01/19/quickly-diff-the-changes-made-in-the-current-buffer-with-its-file/
(defun schnouki/diff-current-buffer-with-file ()
  "Quick diff between current buffer and a file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(bind-key "C-x =" 'schnouki/diff-current-buffer-with-file)

;; Enlarge/shrink window horozontally/vertically
(bind-keys ("C-M-j" . shrink-window)
           ("C-M-k" . enlarge-window)
           ("C-M-h" . shrink-window-horizontally)
           ("C-M-l" . enlarge-window-horizontally))

;; Convert seconds to a duration
(defun schnouki/seconds-to-duration (seconds)
  "Convert seconds to a readable duration."
  (interactive)
  (let* ((secs (if (numberp seconds) seconds
                 (if (stringp seconds) (string-to-number seconds)
                   (error "Argument must be a number or a string"))))
         (h (floor secs 3600))
         (m (floor (mod secs 3600) 60))
         (s (floor (mod secs 60))))
    (concat
     (if (> h 0) (concat (number-to-string h) "h" (if (or (> m 0) (> s 0)) " ")))
     (if (> m 0) (concat (number-to-string m) "m" (if (> s 0) " ")))
     (if (> s 0) (concat (number-to-string s) "s")))))

;; Position function for strings
(defun string-position (item seq)
  "Find the first occurence of ITEM in SEQ.
Return the index of the matching item, or nil if not found."
  (let ((len (length seq))
        (count 0))
    (while (and (< count len) (not (string= item (nth count seq))))
      (setq count (1+ count)))
    (if (= count len) nil count)))

;; Remove *blabla* buffers, except those that match a regexp in the
;; immortal-star-buffers list or a major mode in the immortal-modes list.
(defvar schnouki/immortal-star-buffers nil)
(defvar schnouki/immortal-silent-buffers nil)
(defvar schnouki/immortal-modes nil)
(setq schnouki/immortal-star-buffers `(,(rx string-start "*scratch"
                                            (optional ":" (1+ print))
                                            "*" string-end)
                                       "*pomidor*")
      schnouki/immortal-silent-buffers `(,(rx string-start "*magit:")
                                         "*Messages*")
      schnouki/immortal-modes        '(message-mode notmuch-hello-mode notmuch-search-mode
                                                    notmuch-show-mode inferior-python-mode))

(defun schnouki/buffer-immortal-p (buffer)
  "Check if BUFFER is immortal."
  (let ((buf-name (buffer-name buffer))
        (buf-mode (buffer-local-value 'major-mode buffer)))
    (or
     (--any? (string-match-p it buf-name) schnouki/immortal-star-buffers)
     (-contains? schnouki/immortal-modes buf-mode)
     (and (get-buffer-process buffer) t))))

(defun schnouki/kill-star-buffers (&optional kill-all dry-run)
  "Remove most star-buffers (`*Messages*', `*Compilation', ...)
that are not in the `schnouki/immortal-star-buffers' list.

With prefix argument KILL-ALL, kill all star-buffers. If DRY-RUN
is non-nil, don't actually kill the buffers, but return the list
of buffers that *would* be killed."
  (interactive "P")
  (let ((killed nil))
    (-each (buffer-list)
      (lambda (buf)
        (let ((buf-name (buffer-name buf)))
          (when (and
                 (s-starts-with? "*" buf-name)
                 (or kill-all
                     (not (schnouki/buffer-immortal-p buf))))
            (unless dry-run
              (kill-buffer buf))
            (when (--none? (string-match-p it buf-name) schnouki/immortal-silent-buffers)
              (add-to-list 'killed (cons buf-name buf)))))))
    (cond
     (dry-run killed)
     (killed
      (message "%d buffers killed: %s"
               (length killed)
               (string-join (-map 'car killed)  ", "))))))
(bind-key "C-x M-k" 'schnouki/kill-star-buffers)

;; Kill all buffers visiting files in a directory or its subdirectories.
(defun schnouki/kill-dir-buffers (&optional directory)
  "Remove all buffers visiting DIRECTORY or its subdirectories."
  (interactive "DKill buffers visiting: ")
  (let ((buffers (->> (buffer-list)
                      (--map (cons it (if (eq (buffer-local-value 'major-mode it) 'dired-mode)
                                          (buffer-local-value 'dired-directory it)
                                        (buffer-file-name it))))
                      (--filter (cdr it))
                      (--map (cons (car it) (expand-file-name (cdr it))))
                      (--filter (s-starts-with? directory (cdr it)))
                      (-map 'car))))
    (-each buffers 'kill-buffer)
    (message (format "Killed %d buffers visiting %s" (length buffers) directory))))
(bind-key "k" 'schnouki/kill-dir-buffers schnouki-prefix-map)

;; Kill all buffers in a specific major mode.
(defun schnouki/kill-mode-buffers (&optional mode)
  "Remove all buffers using MODE."
  (interactive
   (let ((all-modes (->> (buffer-list)
                         (--map (buffer-local-value 'major-mode it))
                         (-group-by 'identity))))
     (list
      (completing-read "Kill buffers in major mode: "
                       all-modes nil t nil nil (symbol-name major-mode)))))
  (let ((buffers (->> (buffer-list)
                      (--filter (string= mode
                                         (buffer-local-value 'major-mode it))))))
    (-each buffers 'kill-buffer)
    (message (format "Killed %d buffers using %s" (length buffers) mode))))
(bind-key "K" 'schnouki/kill-mode-buffers schnouki-prefix-map)

(defun schnouki/killable-buffer-list (buffers)
  "Filter and return killable buffers from BUFFERS.
A buffer is considered killable if it is not modified and either visits a file, or is not immortal."
  (--filter (and (buffer-live-p it)
                 (not (buffer-modified-p it))
                 (or (buffer-file-name it)
                     (not (schnouki/buffer-immortal-p it))))
            buffers))

(defun schnouki/sort-buffers-by-display-time (buffers)
  "Sort BUFFERS by display time."
  (--sort (time-less-p (buffer-local-value 'buffer-display-time it)
                       (buffer-local-value 'buffer-display-time other))
          buffers))

(defun schnouki/clean-buffer-list (keep-buffers-nb)
  "Clean buffer list until there are only KEEP-BUFFERS-NB buffers remaining."
  (interactive
   (list
    (or current-prefix-arg
        (let* ((nb-buffers (length (buffer-list)))
               (default-nb (min 100 (/ nb-buffers 2))))
          (read-number (format "Number of buffers (out of %d) to keep: " nb-buffers)
                       default-nb)))))
  (let* ((nb-buffers (length (buffer-list)))
         (nb-buffers-to-kill (- nb-buffers keep-buffers-nb))
         (all-killable-buffers (->> (buffer-list)
                                    (schnouki/killable-buffer-list)
                                    (schnouki/sort-buffers-by-display-time)))
         (star-killed (schnouki/kill-star-buffers nil t))
         (killed-buffers (-map 'cdr star-killed))
         (killable-buffers (-difference all-killable-buffers killed-buffers)))
    (when (< (length killed-buffers) nb-buffers-to-kill)
      (setq killed-buffers (-concat killed-buffers
                                    (-take (- nb-buffers-to-kill (length killed-buffers))
                                           killable-buffers))))
    (when (yes-or-no-p
           (format "About to kill %d buffers: %s. Continue? "
                   (length killed-buffers)
                   (string-join (--map (buffer-name it) killed-buffers) ", ")))
      (--each killed-buffers (kill-buffer it))
      (message "Killed %d buffers (out of %d)." (length killed-buffers) nb-buffers))))
(bind-key "C-x K" 'schnouki/clean-buffer-list)

;; Nicer binding than C-x 5 0 to close the current frame.
(bind-key "C-x w" 'delete-frame)

(defun schnouki/ivy-sort-file-function (x y)
  "Compare two files X and Y.
Prioritize directories, but make sure .dotfiles are last."
  (let ((is-x-dot? (s-starts-with? "." x))
        (is-y-dot? (s-starts-with? "." y)))
    (if (xor is-x-dot? is-y-dot?)
        is-y-dot?
      (ivy-sort-file-function-default x y))))

(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :delight
  :bind (:map schnouki-prefix-map
              ("r " . ivy-resume))
  :custom
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-sort-functions-alist '((t . schnouki/ivy-sort-file-function)))
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (magit-completing-read-function 'ivy-completing-read)
  (projectile-completion-system 'ivy)
  :init
  (defun schnouki/enable-ivy ()
    (ivy-mode 1))
  (add-hook 'after-init-hook #'schnouki/enable-ivy))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :commands (counsel-mode)
  :delight
  :bind (:map counsel-mode-map
              ([remap insert-char] . counsel-unicode-char)
              ([remap switch-to-buffer] . counsel-switch-buffer)
              ([remap switch-to-buffer-other-window] . counsel-switch-buffer-other-window)
              :map schnouki-prefix-map
              ("s" . counsel-rg))
  :custom
  (counsel-find-file-ignore-regexp
   (rx (or (: bos (or "." "#"))
           (: (or "#" "~") eos)
           (: ".pyc" eos))))
  :init
  (defun schnouki/enable-counsel ()
    (counsel-mode 1))
  (add-hook 'after-init-hook #'schnouki/enable-counsel))

;; wgrep
(use-package wgrep
  :ensure t
  :commands wgrep-setup
  :hook (grep-setup . wgrep-setup)
  :custom
  (wgrep-auto-save-buffer t))

;; vundo
(use-package vundo
  :ensure t
  :bind* (("C-'" . undo)
          ("C-\"" . undo-redo))
  :bind (:map ctl-x-map
              ("u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :custom-face
  (vundo-default ((t (:inherit strictly-fixed-pitch)))))

;; goto-last-change
(use-package goto-last-change
  :ensure t
  :bind ("C-x M-u" . goto-last-change))

;; avy (reminder: C-x C-SPC to pop-global-mark)
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
         ("C-M-;" . avy-goto-word-or-subword-1)
         ("C-x C-;" . avy-pop-mark))
  :custom
  (avy-background t)
  :config
  (avy-setup-default)
  :init
  (defhydra hydra-avy (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
^^---------- ^^------------- ^^^^---------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))
  (bind-key ";" 'hydra-avy/body schnouki-prefix-map))


;; Visual feedback on some operations
(use-package volatile-highlights
  :ensure t
  :delight
  :config
  (volatile-highlights-mode t))

;; Google Translate
(use-package google-translate
  :ensure t
  :bind (:map schnouki-prefix-map
              ("W" . google-translate-query-translate))
  :init
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "fr"
        google-translate-enable-ido-completion t))

;; NSFW
(use-package sudoku
  :ensure t
  :commands sudoku
  :custom
  (sudoku-download t)
  (sudoku-level 'medium)
  (sudoku-style 'unicode))

(use-package crossword
  :ensure t
  :commands crossword
  :custom
  (crossword-save-path "~/.cache/crosswords/"))

;; ix.io integration
(use-package ix
  :ensure t
  :commands (ix ix-browse ix-delete))

;; http://www.emacswiki.org/emacs/CamelCase
(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

;; Increment number at point
(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)
  :init
  (defhydra hydra-evil-numbers ()
    "evil numbers:"
    ("="             evil-numbers/inc-at-pt "increase")
    ("<kp-add>"      evil-numbers/inc-at-pt "increase")
    ("-"             evil-numbers/dec-at-pt "decrease")
    ("<kp-subtract>" evil-numbers/dec-at-pt "decrease"))
  (bind-keys :map schnouki-prefix-map
             ("="             . hydra-evil-numbers/evil-numbers/inc-at-pt)
             ("<kp-add>"      . hydra-evil-numbers/evil-numbers/inc-at-pt)
             ("-"             . hydra-evil/numbers/evil-numbers/dec-at-pt)
             ("<kp-subtract>" . hydra-evil/numbers/evil-numbers/dec-at-pt)))

;; Shrink whitespaces
;; http://pragmaticemacs.com/emacs/delete-blank-lines-and-shrink-whitespace/
(use-package shrink-whitespace
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

;; Display Emojis
(use-package emojify
  :ensure t
  :defer t
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-emoji-styles '(unicode))
  (emojify-point-entered-behaviour 'uncover))

;; Colorize strings that represent colors
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :delight)

;; Use ImageMagick as much as possible
;; (let* ((types '(bmp jpeg png svg))
;;        (fix-alist (lambda (alist)
;;                     (--map-when (-contains? types (cdr it))
;;                                 (cons (car it) 'imagemagick)
;;                                 alist))))
;;   (setq image-type-header-regexps (funcall fix-alist image-type-header-regexps)
;;         image-type-file-name-regexps (funcall fix-alist image-type-file-name-regexps)))
;; (fboundp 'imagemagick-types)
;; (imagemagick-types)

(defun schnouki/image-transform-fit-to-window ()
  "Fit the current image to the window."
  ;; From http://emacs.stackexchange.com/a/2458/2006
  (interactive)
  (let* ((img-size (image-display-size (image-get-display-property) t))
         (img-width (car img-size))
         (img-height (cdr img-size))
         (img-h/w-ratio (/ (float img-height) (float img-width)))
         (win-width (window-pixel-width))
         (win-height (window-pixel-height))
         (win-h/w-ratio (/ (float win-height) (float win-width))))
    ;; Fit image by width if the h/w ratio of window is > h/w ratio of the image
    (if (> win-h/w-ratio img-h/w-ratio)
        (image-transform-fit-to-width)
      ;; Else fit by height
      (image-transform-fit-to-height))))

;; Add useful image-mode key bindings
(with-eval-after-load 'image-mode
  (bind-keys :map image-mode-map
             ("r" . image-transform-set-rotation)
             ("zh" . image-transform-fit-to-height)
             ("zs" . image-transform-set-scale)
             ("zw" . image-transform-fit-to-width)
             ("zz" . schnouki/image-transform-fit-to-window)))

;; Weather
(use-package wttrin
  :ensure t
  :bind (:map schnouki-prefix-map
              ("W" . wttrin))
  :init
  (setq wttrin-default-cities '("Nancy" "Forbach" "Paris")))

;; Create directories on the fly
;; http://mbork.pl/2016-07-25_Making_directories_on_the_fly
(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))
(add-hook 'find-file-not-found-functions #'make-parent-directory)

;; EditorConfig
(use-package editorconfig
  :ensure t
  :delight
  :config
  (editorconfig-mode 1))

;; Colorize a buffer using ANSI color codes.
;; If not enough, consider using tty-format.el or xterm-color…
;; https://stackoverflow.com/a/23382008/113325
;; https://github.com/atomontage/xterm-color
(defun ansi-colorize-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Writeroom
(use-package writeroom-mode
  :ensure t
  :bind (:map schnouki-prefix-map
              ("w" . writeroom-mode)
              :map writeroom-mode-map
              ("C-<kp-add>" . writeroom-increase-width)
              ("C-<kp-subtract>" . writeroom-decrease-width)
              ("C-=" . writeroom-adjust-width)))

;; Memory usage
(use-package memory-usage
  :ensure t
  :commands 'memory-usage)

;; Generate UUIDs
(use-package uuidgen
  :ensure t)

;; Frame manipulation
(use-package transpose-frame
  :ensure t
  :bind (:map schnouki-prefix-map
              ("p t" . transpose-frame)
              ("p f" . flip-frame)
              ("p F" . flop-frame)
              ("p r" . rotate-frame-clockwise)
              ("p R" . rotate-frame-anticlockwise)))

;; Keep init-00-custom up-to-date! :)
(defun schnouki/update-selected-packages ()
  (interactive)
  (let* ((output (shell-command-to-string "rg --no-filename --no-line-number --no-heading '^\\(use-package' ~/.config/emacs | awk '{print $2}' | sort -u"))
         (lines (s-lines (s-trim output)))
         (packages (-map 'intern lines)))
    (customize-save-variable 'package-selected-packages packages)))
;; (schnouki/update-selected-packages)
;; (package-autoremove)
;; (package-install-selected-packages)

;; Refresh environment variables, e.g. when sway is reloaded
(defun schnouki/refresh-env ()
  (require 'dash)
  (require 's)
  (let ((env-vars '("SWAYSOCK" "I3SOCK")))
    (-as-> (with-output-to-string
             (with-current-buffer standard-output
               (call-process "systemctl" nil t nil "--user" "show-environment")))
           env
           (s-lines env)
           (-remove #'s-blank-str? env)
           (--map (s-split-up-to "=" it 1) env)
           (--filter (-contains? env-vars (car it)) env)
           (--each env (apply #'setenv it)))))

;; GUI for pueue
(use-package pueue
  :ensure t)

;; Actionable URLs in buffers
;; https://xenodium.com/actionable-urls-in-emacs-buffers/
(use-package goto-addr
  :hook ((prog-mode . goto-address-prog-mode)
         (compilation-mode . goto-address-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode)
         (magit-process-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("RET" . goto-address-at-point)
              ("M-RET" . newline)))

;; Transparent age encryption support
(use-package age
  :ensure t
  :defer t
  :custom
  (age-program "rage")
  (age-default-identity '("~/.config/age/age-yubikey-identity-fd6d28aa.txt"
                          "~/.config/age/age-yubikey-identity-ff36b8b1.txt"))
  (age-default-recipient '("~/.config/age/age-yubikey-fd6d28aa.pub"
                           "~/.config/age/age-yubikey-ff36b8b1.pub"
                           "~/.config/age/age-7qlgd5cs.pub"))
  :config
  (age-file-enable))

;;; init-25-utils.el ends here
