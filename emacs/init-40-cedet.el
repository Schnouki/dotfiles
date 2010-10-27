;; -----------------------------------------------------------------------------
;; CEDET 
;; -----------------------------------------------------------------------------

(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")

;; Project management - EDE package
(global-ede-mode t)

;; Semantic code helpers
;; (probably redundant with which-func-mode which is already enabled somewhere else)
(semantic-load-enable-excessive-code-helpers)

;; To use additional features for names completion, and displaying of
;; information for tags & classes, you also need to load the semantic-ia package
;(require 'semantic-ia)

;; Access to system include files
(require 'semantic-gcc)
