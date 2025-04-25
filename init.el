;; MELPA has more up-to-date packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Built-in Emacs customizations

;; Store customizations in another file to keep init.el clean
(setq custom-file (locate-user-emacs-file "custom-sets.el"))
(load custom-file 'noerror 'nomessage)

;; GUI cleanup
(tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)

;; Better completions
;; See: https://robbmann.io/posts/emacs-29-completions/
(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab)
(setq tab-always-indent 'complete)

;; No weird files
(auto-save-visited-mode 1)
(setq make-backup-files nil)

;; Remember...
;; Remember recently opened files
(recentf-mode 1)
;; Remember recent minibuffer entries, such as M-x commands
(savehist-mode 1)
;; Remember cursor location in previously opened files
(save-place-mode 1)

;; Automatically update buffers if the underlying file changes
(global-auto-revert-mode 1)

;; Highlight trailing whitespace; it's buffer local so we change the default
(setq-default show-trailing-whitespace t)

;; Show match count in isearch
(setq isearch-lazy-count t)

;; Profile startup with use-package; run use-package-report to see results
;; (setq use-package-compute-statistics t)

;;; Third-party packages
