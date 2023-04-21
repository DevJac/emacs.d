;;; straight.el bootstrap
;; See: https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Vanilla Emacs config
;; 16 point font
(set-face-attribute 'default nil :height 160)
;; tool-bar-mode show icons, menu-bar-mode show text menu dropdowns
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
;; Remember the files we've opened recently
(recentf-mode 1)
;; Save minibuffer history, making frequently used commands easier to access
(savehist-mode 1)
;; Scrolling begins this many lines from edge
(setq scroll-margin 2)
;; Never center the point because of scrolling
(setq scroll-conservatively 101)
;; Number of lines of continuity when scrolling by screenfuls
(setq next-screen-context-lines 4)
;; Display column number on modeline
(column-number-mode 1)
;; Auto save to visited file instead of a backup
(auto-save-visited-mode 1)
;; Auto revert file when it changes from some outside source
(global-auto-revert-mode 1)
;; No backup files
(setq make-backup-files nil)

;;; straight integration with use-package
;; See: https://github.com/radian-software/straight.el#integration-with-use-package
;; We want all normal (use-package ...) expressions to use straight by default
(setq straight-use-package-by-default t)
;; Before we can use normal (use-package ...) expressions, we need to get use-package
(straight-use-package 'use-package)

(add-to-list
 'load-path
 (expand-file-name "elisp" user-emacs-directory))
(load "defuns")

;;; use-package's
;; :init runs before the package is loaded
;; :config runs after the package is loaded
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))
;; Emacs 29 will have a restart function; maybe remove later?
(use-package restart-emacs)
(use-package evil
  :init
  ;; When searching, keep search highlights visible
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  :config
  (load "evil-keys")
  (evil-mode 1))
;; evil-anzu is used to show search match count in modeline
(use-package evil-anzu
  :config
  (global-anzu-mode 1))
(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-histories")))
  :config
  (global-undo-tree-mode 1))
(use-package poke-line
  :config
  (poke-line-global-mode 1))
(use-package doom-modeline
  :init
  ;; Select info shows word count
  (setq doom-modeline-enable-word-count t)
  :config
  (doom-modeline-mode 1))
;; consult provides specific commands, like consult-buffer
(use-package consult)
;; vertico provides the selection UI
(use-package vertico
  :config
  (vertico-mode 1))
;; orderless provides the fuzzy string matching
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))
;; marginalia provides added info in right margins
(use-package marginalia
  :config
  (marginalia-mode 1))
(use-package org
  :init
  (setq org-clock-idle-time 15)
  (setq org-clock-mode-line-total 'current)
  (setq org-duration-format 'h:mm)
  (setq org-ellipsis " ▼")
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-log-done 'time)
  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (add-hook 'whitespace-mode-hook #'fix-org-ellipsis-after-whitespace-mode))
(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
(use-package which-key
  :config
  (which-key-mode 1))

;;; Mac / Homebrew config
; (add-to-list 'exec-path "/opt/homebrew/bin")

;;; Customs
;; Darkened highlight and region backgrounds by #101010
;; Removed region distant-foreground; it was an ugly bright color
;; Lightened comment face by #101010
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#8c7f74"))))
 '(highlight ((t (:background "#403935"))))
 '(org-block-begin-line ((t (:foreground "#8c7f74"))))
 '(org-block-end-line ((t (:foreground "#8c7f74"))))
 '(region ((t (:background "#403935"))))
 '(show-paren-match ((t (:foreground nil :background "#403935" :weight bold)))))
