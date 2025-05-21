;; MELPA has more up-to-date packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Store customizations in another file to keep init.el clean
(setq custom-file (locate-user-emacs-file "custom-sets.el"))
(load custom-file 'noerror 'nomessage)
;; GUI cleanup
(load-theme 'modus-operandi)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; Better built-in completions; see: https://robbmann.io/posts/emacs-29-completions/
(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab)
;; TAB first tries to indent, then tries to complete
(setq tab-always-indent 'complete)
;; No weird files
(auto-save-visited-mode 1)
(setq make-backup-files nil)
;; Remember recently opened files
(recentf-mode 1)
;; Remember recent minibuffer entries, such as M-x commands
(savehist-mode 1)
;; Remember cursor location in previously opened files
(save-place-mode 1)
;; Automatically update buffers if the underlying file changes
(global-auto-revert-mode 1)
;; Show match count in isearch
(setq isearch-lazy-count t)
;; Display keybindings
(which-key-mode 1)
;; Don't indent with TABs; this is buffer local so we change the default
(setq-default indent-tabs-mode nil)
;; Indent 4 spaces
(setq c-basic-offset 4)
;; Profile startup with use-package; run use-package-report to see results
;; (setq use-package-compute-statistics t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(load "purr.el")
(load "my.el")
(global-purr-mode 1)

(use-package consult
  :ensure t
  :bind
  ("C-x b" . #'consult-buffer)
  :bind*
  ("C-M-i" . #'my/complete-in-minibuffer)
  :config
  (consult-customize
   consult-buffer consult-project-buffer
   consult-grep consult-git-grep consult-ripgrep
   consult-find consult-fd
   consult-theme
   :preview-key '(:debounce 0.5 any)))
(use-package corfu
  :ensure t
  :config
  (corfu-history-mode 1)
  (global-corfu-mode 1))
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))
(use-package orderless
  :ensure t
  :custom
  ;; These customizations are suggested by the orderless docs.
  ;; Apparently file completion in TRAMP must use basic; see docs for more info.
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))
(use-package magit
  :ensure t)
(use-package expreg
  :ensure t
  :config
  (load "my-expreg-functions.el"))
(use-package gptel
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package org
  :ensure t
  :init
  (setq org-ellipsis " ▼")
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil))
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory "~/OrgRoam/"))
(use-package olivetti
  :ensure t)
