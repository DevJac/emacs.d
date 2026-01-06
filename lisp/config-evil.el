(use-package goto-chg
  :ensure t)
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  :config
  (global-undo-tree-mode))
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-overriding-maps nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init 'dired))
