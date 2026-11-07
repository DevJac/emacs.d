;; -*- lexical-binding: t; -*-

(use-package goto-chg
  :ensure t)
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :ensure t
  :init
  (setq evil-collection-key-blacklist '("SPC"))
  :config
  (evil-collection-init '(dired magit)))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode 1))
(use-package evil-anzu
  :ensure t)
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-enable-word-count t)
  (doom-modeline-mode 1))

;; Don't allow evil commands to quit Emacs.
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window-quiet)
(evil-ex-define-cmd "wq" #'save-then-kill-buffer-and-window-quiet)
(evil-define-key '(motion normal) 'global
  (kbd "ZQ") #'kill-buffer-and-window-quiet
  (kbd "ZZ") #'save-then-kill-buffer-and-window-quiet)

(require 'my-evil)

;; These help when navigating display overlays.
(advice-add 'evil-next-line :around #'my/evil-next-line-advice)
(advice-add 'evil-previous-line :around #'my/evil-previous-line-advice)

;; Create a text object between |'s. This is especially useful for org tables.
(define-key evil-inner-text-objects-map "|" #'evil-org-table-cell-inner)
