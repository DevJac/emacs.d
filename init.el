;;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Theme config
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(set-face-attribute 'default nil :height 160)
(setq scroll-margin 2)
(setq scroll-conservatively 100)
(setq next-screen-context-lines 4)
(column-number-mode 1)

;;; Packages
(straight-use-package 'use-package)
(use-package restart-emacs ; keep first, useful in case remaining config is bad
  :straight t)
(use-package gruvbox-theme ; keep early, we want to set the theme ASAP
  :straight t
  :config
  (load-theme 'gruvbox-dark-hard t))
(use-package emacs ; keep early, special
  :load-path "elisp"
  :config
  (load "defuns"))
(use-package avy
  :straight t)
(use-package counsel
  :straight t
  :config
  (counsel-mode 1))
(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count t)
  :config
  (doom-modeline-mode 1))
(use-package evil
  :straight t
  :load-path "elisp"
  :init
  (setq evil-undo-system 'undo-tree)
  :config
  (load "evil-keys")
  (evil-mode 1))
(use-package expand-region
  :straight t)
(use-package ivy
  :straight t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist
	'((counsel-M-x . "")
          (counsel-describe-function . "")
          (counsel-describe-variable . "")))
  :config
  (global-set-key (kbd "C-s") #'swiper)
  (ivy-mode 1))
(use-package ivy-prescient
  :straight t
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))
(use-package keyfreq
  :straight t
  :init
  (setq keyfreq-file "~/.emacs.d/keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
(use-package ledger-mode
  :straight t)
(use-package minions
  :straight t
  :config
  (minions-mode 1))
(use-package org-drill
  :straight t
  :init
  (setq org-drill-hide-item-headings-p t)
  (setq org-drill-add-random-noise-to-intervals-p t))
(use-package projectile
  :straight t
  :init
  (setq projectile-completion-system 'ivy))
(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))
(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

;;; File config
(auto-save-visited-mode 1)
(global-auto-revert-mode 1)
(setq make-backup-files nil)

;;; Org config
(setq org-clock-idle-time 15)
(setq org-clock-mode-line-total 'current)
(setq org-fontify-done-headline nil)
(setq org-fontify-todo-headline nil)
(setq org-ellipsis "⤵")
;; org-mode and whitespace-mode both modify Emacs' "display tables".
;; When leaving whitespace-mode, my custom org-ellipsis were being replaced with
;; the standard "...". To fix this, I reapply the display table modifications
;; made by org-mode when leaving whitespace-mode.
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Tables.html
(add-hook 'whitespace-mode-hook #'fix-org-ellipsis-after-whitespace-mode)
;; When we scale text, we want our rendered latex fragments to scale as well.
(add-hook 'org-mode-hook 'scale-latex-fragments)
(add-hook 'text-scale-mode-hook
          (lambda () (when (eq major-mode 'org-mode) (scale-latex-fragments))))

;;; Custom
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:background nil :foreground "#7c6f64"))))
 '(org-block-end-line ((t (:background nil :foreground "#7c6f64"))))
 '(org-drawer ((t (:background nil :foreground "#7c6f64"))))
 '(org-code ((t (:background "#32302f" :foreground nil)))))
