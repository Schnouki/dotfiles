;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Simple Python Completion Source for Auto-Complete
;;;;; =================================================
;;;;;
;;;;; This file provides a completion source for Auto-Complete:
;;;;; http://www.emacswiki.org/emacs/AutoComplete
;;;;;
;;;;; Installation
;;;;; ------------
;;;;; 
;;;;; Setup Auto-Complete in the usual fashion, and make sure it gets loaded for
;;;;; python buffers. Then, place this file in your load-path, and add
;;;;; 
;;;;;     (require 'ac-python)
;;;;; 
;;;;; to your .emacs file (after loading Auto-Complete).
;;;;; 
;;;;; Usage
;;;;; -----
;;;;; 
;;;;; Python symbols will be completed by Auto-Complete, once Emacs learns about
;;;;; these symbols. This is the short-coming of the plugin, but it's a small
;;;;; price to pay.
;;;;; 
;;;;; To teach Emacs about symbols in imported modules, Emacs needs to execute
;;;;; the Python source. This can be accomplished with `python-send-buffer` for
;;;;; example, often bound to `C-c C-c`. If a python process is already running,
;;;;; this is essentially instantaneous.
;;;;;
;;;;; ---
;;;;;
;;;;; Version: 20110519
;;;;; License: MIT
;;;;; Author: Chris Poole <chris@chrispoole.com>
;;;;; More information: http://chrispoole.com/project/ac-python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ac-get-python-symbol-at-point ()
  "Return python symbol at point.

Assumes symbol can be alphanumeric, `.' or `_'."
  (let ((end (point))
        (start (ac-python-start-of-expression)))
    (buffer-substring-no-properties start end)))

(defun ac-python-completion-at-point ()
  "Returns a possibly empty list of completions for the symbol at
point."
  (python-symbol-completions (ac-get-python-symbol-at-point)))

(defun ac-python-start-of-expression ()
  "Return point of the start of python expression at point.

Assumes symbol can be alphanumeric, `.' or `_'."
  (save-excursion
    (and (re-search-backward
          (rx (or buffer-start (regexp "[^[:alnum:]._]"))
              (group (1+ (regexp "[[:alnum:]._]"))) point)
          nil t)
         (match-beginning 1))))

(defvar ac-source-python
  '((candidates . ac-python-completion-at-point)
    (prefix . ac-python-start-of-expression)
    (symbol . "f")
    (requires . 2))
  "Source for python completion.")

(add-hook 'python-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-python)))

(provide 'ac-python)