;; -*- lexical-binding: t; -*-

;; See: https://github.com/emacs-evil/evil

;; goto-chg is required for goto-last-change, etc; bound to g; and g,
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
  ;; evil-collection assumes evil-want-keybinding is nil
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  ;; TAB and C-i are the same; this prevents TAB from jumping:
  (setq evil-want-C-i-jump nil)
  ;; Many things in Emacs assume the cursor can move past the eol;
  ;; it's not a big deal, let's stop fighting it.
  (setq evil-move-beyond-eol t)
  :config
  (evil-mode 1))
(use-package evil-collection
  :ensure t
  :init
  (setq evil-collection-key-blacklist '("SPC"))
  :config
  (evil-collection-init '(dired magit)))
;; This unusual evil-org config comes from the README.
;; See: https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
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
